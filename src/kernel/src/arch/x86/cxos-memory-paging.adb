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

with x86.Memory.Paging;

package body Cxos.Memory.Paging is
   ----------------------------------------------------------------------------
   --  Create_New_Page_Directory
   --
   --  Purpose:
   --    Allocates and initialises a new page directory, placing the address
   --    of the newly allocated directory in the provided parameter.
   ----------------------------------------------------------------------------
   function Create_New_Page_Directory (
     Page_Directory_Addr : out System.Address
   ) return Process_Result is
   begin
      Page_Directory_Addr := To_Address (0);
      return Success;
   end Create_New_Page_Directory;

   ----------------------------------------------------------------------------
   --  Find_Free_Kernel_Page
   ----------------------------------------------------------------------------
   function Find_Free_Kernel_Page (
     Table_Index : out Natural;
     Page_Index  : out Natural
   ) return Process_Result is
      use x86.Memory.Paging;

      --  The currently loaded kernel page_directory.
      Kernel_Page_Dir : Page_Directory
      with Import,
        Convention => Ada,
        Address    => To_Address (PAGE_DIR_MAPPED_ADDR);

      --  The address of each page table.
      --  This will be set to the address that each page table being checked
      --  is recursively mapped into memory.
      Table_Addr : System.Address;

      --  The result of internal processes.
      Result : Process_Result;
   begin
      --  Loop over every page in the directory, checking only the entries
      --  which are marked as present.
      --  The last directory entry is ignored, since this is the recursive
      --  mapping entry.
      for Dir_Entry_Idx in Integer range 768 .. 1022 loop
         if Kernel_Page_Dir (Dir_Entry_Idx).Present then
            --  Get the address of this page table in memory.
            Result := Get_Page_Table_Mapped_Address (Dir_Entry_Idx,
              Table_Addr);
            if Result /= Success then
               return Unhandled_Exception;
            end if;

            --  Check the page table for non-present entries, denoting a free
            --  page frame.
            Check_Page_Table :
               declare
                  --  The page table to check.
                  Kernel_Table : constant Page_Table
                  with Import,
                    Convention => Ada,
                    Address    => Table_Addr;
               begin
                  --  Check each frame entry in the page table.
                  for Frame_Idx in Integer range 0 .. 1023 loop
                     if Kernel_Table (Frame_Idx).Present = False then
                        Table_Index := Dir_Entry_Idx;
                        Page_Index  := Frame_Idx;

                        return Success;
                     end if;
                  end loop;
               end Check_Page_Table;
         end if;
      end loop;

      --  If we have not found an address, set the output to NULL and return
      --  that there are no free frames.
      Table_Index := 0;
      Page_Index  := 0;

      return No_Free_Frames;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Find_Free_Kernel_Page;

   ----------------------------------------------------------------------------
   --  Get_Page_Table_Mapped_Address
   ----------------------------------------------------------------------------
   function Get_Page_Table_Mapped_Address (
     Table_Index :     Natural;
     Mapped_Addr : out System.Address
   ) return Process_Result is
      --  The offset from the base mapping offset.
      Table_Map_Offset : Integer_Address := 0;
   begin
      Calculate_Mapped_Address :
         begin
            Table_Map_Offset := Integer_Address (16#1000# * Table_Index);
            Mapped_Addr := To_Address (PAGE_TABLES_MAPPED_ADDR
              + Table_Map_Offset);
         exception
            when Constraint_Error =>
               return Unhandled_Exception;
         end Calculate_Mapped_Address;

      return Success;
   end Get_Page_Table_Mapped_Address;

   ----------------------------------------------------------------------------
   --  Get_Page_Table_Mapped_Address
   ----------------------------------------------------------------------------
   function Get_Page_Table_Mapped_Address (
     Virtual_Addr :     System.Address;
     Mapped_Addr  : out System.Address
   ) return Process_Result is
      use x86.Memory.Paging;

      --  The index into the page directory that this virtual address
      --  is mapped at.
      Directory_Idx    : Natural;
      --  The offset from the base mapping offset.
      Table_Map_Offset : Integer_Address := 0;
      --  The result of internal processes.
      Result           : x86.Memory.Paging.Process_Result;
   begin
      --  Ensure that the provided address is properly page aligned.
      Check_Address :
         begin
            if not Check_Address_Page_Aligned (Virtual_Addr) then
               return Invalid_Non_Aligned_Address;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Non_Aligned_Address;
         end Check_Address;

      --  Get the directory index.
      Get_Directory_Idx :
         begin
            Result := Get_Page_Directory_Index (Virtual_Addr, Directory_Idx);
            if Result /= Success then
               return Unhandled_Exception;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Non_Aligned_Address;
         end Get_Directory_Idx;

      Calculate_Mapped_Address :
         begin
            Table_Map_Offset := Integer_Address (16#1000# * Directory_Idx);
            Mapped_Addr := To_Address (PAGE_TABLES_MAPPED_ADDR
              + Table_Map_Offset);
         exception
            when Constraint_Error =>
               return Unhandled_Exception;
         end Calculate_Mapped_Address;

      return Success;
   end Get_Page_Table_Mapped_Address;

end Cxos.Memory.Paging;
