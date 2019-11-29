-------------------------------------------------------------------------------
--  Copyright (c) 2019, CXOS.
--  This program is free software; you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3 of the License, or
--  (at your option) any later version.
--
--  Authors:
--     Anthony <ajxs [at] panoptic.online>
-------------------------------------------------------------------------------

with System.Storage_Elements;
with Cxos.Memory.Map;

package body Cxos.Memory.Paging is
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Allocate_Page_Frame
   ----------------------------------------------------------------------------
   function Allocate_Page_Frame (
     Virtual_Address :     System.Address;
     Frame_Address   : out x86.Memory.Paging.Page_Aligned_Address
   ) return Process_Result is
   begin
      pragma Unreferenced (Virtual_Address);
      pragma Unreferenced (Frame_Address);

      return Success;
   end Allocate_Page_Frame;

   ----------------------------------------------------------------------------
   --  Insert_Page_Table
   ----------------------------------------------------------------------------
   function Insert_Page_Table (
     Directory_Idx : Natural;
     Supervisor    : Boolean := True
   ) return Process_Result is
      use Cxos.Memory.Map;
      use x86.Memory.Paging;

      --  The currently active page directory.
      Directory : Page_Directory
      with Import,
        Convention => Ada,
        Address    => To_Address (16#FFFF_F000#);

      --  The address of the newly allocated page frame, if applicable.
      Allocated_Addr     : System.Address;
      --  The virtual address of the newly created table.
      Table_Virtual_Addr : System.Address;
   begin
      --  If this entry has already been allocated, abort.
      if Directory (Directory_Idx).Present = True then
         return Invalid_Argument;
      end if;

      --  Allocate the new page frame needed for the table.
      Allocate_New_Frame :
         declare
            --  The process result of allocating a new page frame, if needed.
            Allocate_Result : Process_Result;
         begin
            --  Allocate a page frame for the new page table.
            Allocate_Result := Cxos.Memory.Map.Allocate_Frame (Allocated_Addr);
            if Allocate_Result /= Success then
               return Frame_Allocation_Error;
            end if;
         end Allocate_New_Frame;

      --  Set the address at the applicable index into the page
      --  directory to point to the newly allocated page table.
      Directory (Directory_Idx).Table_Address :=
        Convert_To_Page_Aligned_Address (Allocated_Addr);
      Directory (Directory_Idx).Present := True;

      --  If this is a user level page, set this permission level.
      if Supervisor = False then
         Directory (Directory_Idx).U_S := True;
      end if;

      --  Refresh the page directory to allow the use of our
      --  new mapping.
      Flush_Tlb;

      --  Calculate the virtual address of the page table.
      --  The virtual address will always be a constant referring to the
      --  recursively mapped table in the table index.
      Get_Table_Virtual_Address :
         declare
            Table_Map_Base   : constant Integer_Address := 16#FFC0_0000#;
            Table_Map_Offset : Integer_Address := 0;
         begin
            Table_Map_Offset   := Integer_Address (16#1000# * Directory_Idx);
            Table_Virtual_Addr :=
              To_Address (Table_Map_Base + Table_Map_Offset);
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Get_Table_Virtual_Address;

      --  Initialise the newly allocated page table.
      Init_Table :
         declare
            --  The result of internal processes.
            Result : x86.Memory.Paging.Process_Result;
            --  The newly allocated page table.
            Table : Page_Table
            with Import,
              Convention => Ada,
              Address    => Table_Virtual_Addr;
         begin
            --  Initialise the new page table.
            Result := Initialise_Page_Table (Table);
            if Result /= Success then
               return Unhandled_Exception;
            end if;
         end Init_Table;

      return Success;
   exception
      when Constraint_Error =>
         return Invalid_Table_Index;
   end Insert_Page_Table;

   ----------------------------------------------------------------------------
   --  Map_Page_Frame
   --
   --  Implementation Notes:
   --    - Maps the frame in the currently loaded virtual address space.
   --    - Assumes the paging directory is recursively mapped.
   ----------------------------------------------------------------------------
   function Map_Page_Frame (
     Physical_Addr : System.Address;
     Virtual_Addr  : System.Address
   ) return Process_Result is
      use x86.Memory.Paging;

      --  The currently loaded page directory.
      Directory : Page_Directory
      with Import,
        Convention => Ada,
        Address    => To_Address (16#FFFF_F000#);

      --  Index variables used when mapping  the page directory and table.
      Directory_Idx : Natural;
      Table_Idx     : Natural;
      --  The virtual address of the table to map the page frame in.
      Table_Virtual_Addr : System.Address;
   begin
      --  Ensure that the provided addresses are 4K aligned.
      Check_Address :
         begin
            if (not Check_Address_Page_Aligned (Physical_Addr)) or
              (not Check_Address_Page_Aligned (Virtual_Addr))
            then
               return Invalid_Non_Aligned_Address;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Non_Aligned_Address;
         end Check_Address;

      --  Ensure that this page directory has been properly recursively mapped.
      --  This will be necessary to add a new page table if necessary.
      Check_Page_Directory :
         begin
            if Directory (1023).Present = False then
               return Invalid_Page_Directory;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Page_Directory;
         end Check_Page_Directory;

      --  Get the indexes into the page directory and page table.
      Get_Indexes :
         declare
            --  Result variable for internal processes.
            Result : x86.Memory.Paging.Process_Result;
         begin
            Result := Get_Page_Directory_Index (Virtual_Addr, Directory_Idx);
            if Result /= Success then
               return Unhandled_Exception;
            end if;

            Result := Get_Page_Table_Index (Virtual_Addr, Table_Idx);
            if Result /= Success then
               return Unhandled_Exception;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Get_Indexes;

      --  Calculate the virtual address of the page table.
      --  The virtual address will always be a constant referring to the
      --  recursively mapped table in the table index.
      Get_Table_Virtual_Address :
         declare
            --  Result variable for internal processes.
            Result : x86.Memory.Paging.Process_Result;
         begin
            Result := Get_Page_Table_Mapped_Address (Virtual_Addr,
              Table_Virtual_Addr);
            if Result /= Success then
               return Unhandled_Exception;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Get_Table_Virtual_Address;

      --  Check that the table mapping in the directory is present, create it
      --  if not.
      Check_Table_Mapping :
         declare
            --  The result of inserting a new page table record, if needed.
            Insert_Result : Process_Result;
         begin
            --  If there is no entry currently at this index in the page
            --  directory, allocate a new frame for to hold this page table,
            --  then allocate and initialise the new page table.
            if not Directory (Directory_Idx).Present then
               Insert_Result := Insert_Page_Table (Directory_Idx);
               if Insert_Result /= Success then
                  return Insert_Result;
               end if;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Check_Table_Mapping;

      Map_Entry :
         declare
            --  The page table to map the entry in.
            Table : Page_Table
            with Import,
              Convention => Ada,
              Address    => Table_Virtual_Addr;
         begin
            Table (Table_Idx).Page_Address :=
              Convert_To_Page_Aligned_Address (Physical_Addr);
            Table (Table_Idx).Present      := True;
         exception
            when Constraint_Error =>
               return Unhandled_Exception;
         end Map_Entry;

      return Success;
   end Map_Page_Frame;
end Cxos.Memory.Paging;
