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

with Interfaces;
with System.Storage_Elements;

package body x86.Paging is
   use Interfaces;
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Check_Address_Aligned
   ----------------------------------------------------------------------------
   function Check_Address_Aligned (
     Addr : System.Address
   ) return Boolean is
   begin
      return (To_Integer (Addr) and 16#FFF#) = 0;
   end Check_Address_Aligned;

   ----------------------------------------------------------------------------
   --  Convert_To_Aligned_Address
   --
   --  Implementation Notes:
   --   - Converts the address to a 32 bit unsigned integer in order to
   --     properly truncate the value to the 4kb aligned 20-bit value.
   ----------------------------------------------------------------------------
   function Convert_To_Aligned_Address (
     Addr : System.Address
   ) return Aligned_Address is
      Address_As_Unsigned : Unsigned_32;
   begin
      Address_As_Unsigned := Unsigned_32 (To_Integer (Addr));
      Address_As_Unsigned := Address_As_Unsigned and 16#FFFFF000#;
      Address_As_Unsigned := Shift_Right (Address_As_Unsigned, 12);

      return Aligned_Address (Address_As_Unsigned);
   exception
      when Constraint_Error =>
         return 0;
   end Convert_To_Aligned_Address;

   ----------------------------------------------------------------------------
   --  Convert_To_System_Address
   ----------------------------------------------------------------------------
   function Convert_To_System_Address (
     Addr : Aligned_Address
   ) return System.Address is
      Address_As_Unsigned : Unsigned_32;
   begin
      Address_As_Unsigned := Unsigned_32 (Addr);
      Address_As_Unsigned := Shift_Right (Address_As_Unsigned, 12);

      return To_Address (Integer_Address (Address_As_Unsigned));
   exception
      when Constraint_Error =>
         return System.Null_Address;
   end Convert_To_System_Address;

   ----------------------------------------------------------------------------
   --  Get_Page_Directory_Index
   ----------------------------------------------------------------------------
   function Get_Page_Directory_Index (
     Addr    : System.Address;
     Index   : out Natural
   ) return Paging_Process_Result is
      Addr_As_Uint : Unsigned_32;
   begin
      --  Ensure that the provided address is 4K aligned.
      Check_Address :
         begin
            if not Check_Address_Aligned (Addr) then
               return Invalid_Non_Aligned_Address;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Non_Aligned_Address;
         end Check_Address;

      --  Return only highest-order 10 bits.
      Addr_As_Uint := Unsigned_32 (To_Integer (Addr));
      Addr_As_Uint := Shift_Right (Addr_As_Uint, 22);

      --  Convert the resulting value to a valid index value.
      Convert_To_Natural :
         begin
            Index := Natural (Addr_As_Uint);

            if not Index'Valid then
               raise Constraint_Error;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Convert_To_Natural;

      return Success;
   end Get_Page_Directory_Index;

   ----------------------------------------------------------------------------
   --  Get_Page_Table_Index
   ----------------------------------------------------------------------------
   function Get_Page_Table_Index (
     Addr    : System.Address;
     Index   : out Natural
   ) return Paging_Process_Result is
      Addr_As_Uint : Unsigned_32;
   begin
      --  Ensure that the provided address is 4K aligned.
      Check_Address :
         begin
            if not Check_Address_Aligned (Addr) then
               return Invalid_Non_Aligned_Address;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Non_Aligned_Address;
         end Check_Address;

      Addr_As_Uint := Unsigned_32 (To_Integer (Addr));
      Addr_As_Uint := Shift_Right (Addr_As_Uint, 12) and 16#03FF#;

      --  Convert the resulting value to a valid index value.
      Convert_To_Natural :
         begin
            Index := Natural (Addr_As_Uint);

            if not Index'Valid then
               raise Constraint_Error;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Convert_To_Natural;

      return Success;
   end Get_Page_Table_Index;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Implementation Notes:
   --   - Initialises every page table as being non present and non writeable.
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      --  Initialise the page table structure.
      --  Initially all tables are marked as non-present.
      Initialise_Page_Tables :
--         declare
--            Current_Address : Unsigned_32 := 0;
         begin
            --  Initialise each table in the page table structure.
            for Table of Page_Tables loop
               --  Initialise each entry in this page table.
               for Idx in Table'Range loop
                  Table (Idx).Present      := True;
                  Table (Idx).Read_Write   := True;
                  Table (Idx).U_S          := False;
                  Table (Idx).PWT          := False;
                  Table (Idx).PCD          := False;
                  Table (Idx).A            := False;

                  Current_Address := Unsigned_32 (Idx) * 16#1000#;

                  --  Shift the address right 12 bits to fit the
                  --  20bit format.
                  Table (Idx).Page_Address :=
                    Physical_Page_Address (Shift_Right (
                    Current_Address, 12));

                  --  Table (Idx) := (Unsigned_32 (Idx) * 16#1000#) or 3;
               end loop;
            end loop;
         exception
            when Constraint_Error =>
               return;
         end Initialise_Page_Tables;

      --  Initialises all of the page directory entries.
      --  This correctly points each entry at the relevant page table.
      Initialise_Page_Directory :
         begin
            for Idx in Page_Directory'Range loop
               Page_Directory (Idx).Present       := True;
               Page_Directory (Idx).Read_Write    := True;
               Page_Directory (Idx).U_S           := False;
               Page_Directory (Idx).PWT           := False;
               Page_Directory (Idx).PCD           := False;
               Page_Directory (Idx).A             := False;
               Page_Directory (Idx).PS            := False;
               Page_Directory (Idx).G             := False;

               Page_Directory (Idx).Table_Address := Page_Table_Address (
                 Convert_To_Aligned_Address (Page_Tables (Idx)'Address));

            end loop;
         exception
            when Constraint_Error =>
               return;
         end Initialise_Page_Directory;

   end Initialise;

   ----------------------------------------------------------------------------
   --  Map_Page_Table_Entry
   --
   --  Purpose:
   --    This procedure maps a specific page table entry to a virtual address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Map_Page_Table_Entry (
     Directory        : in out Page_Directory_Array;
     Physical_Address : System.Address;
     Virtual_Address  : System.Address
   ) return Paging_Process_Result is
      Directory_Idx  : Natural;
      Table_Idx      : Natural;
      Process_Result : Paging_Process_Result;
      Table_Addr     : System.Address;
      Page_Addr      : Physical_Page_Address;
   begin
      --  Ensure that the provided addresses are 4K aligned.
      Check_Address :
         begin
            if (not Check_Address_Aligned (Physical_Address)) or
              (not Check_Address_Aligned (Virtual_Address))
            then
               return Invalid_Non_Aligned_Address;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Non_Aligned_Address;
         end Check_Address;

      --  Get the indexes into the page directory and page table.
      Get_Indexes :
         begin
            Process_Result := Get_Page_Directory_Index (Virtual_Address,
              Directory_Idx);
            if Process_Result /= Success then
               return Process_Result;
            end if;

            Process_Result := Get_Page_Table_Index (Virtual_Address,
              Table_Idx);
            if Process_Result /= Success then
               return Process_Result;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Get_Indexes;

      --  Check that the relevant page frame exists and create it if needed.
      Check_Frame :
         begin
            if not Directory (Directory_Idx).Present then
               Directory (Directory_Idx).Present := True;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Check_Frame;

      Get_Table_Address :
         begin
            Table_Addr := Convert_To_System_Address (
              Aligned_Address (Directory (Directory_Idx).Table_Address));
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Get_Table_Address;

      Map_Entry :
         declare
            Table : Page_Table
            with Import,
              Convention => Ada,
              Address    => Table_Addr;
         begin
            Page_Addr := Physical_Page_Address (
              Convert_To_Aligned_Address (Physical_Address));

            Table (Table_Idx).Page_Address := Page_Addr;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Map_Entry;

      return Success;
   end Map_Page_Table_Entry;
end x86.Paging;
