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
         declare
            Current_Address : Unsigned_32 := 0;
         begin
            --  Initialise each table in the page table structure.
            for Table of Page_Tables loop
               --  Initialise each entry in this page table.
               for PT_Entry of Table loop
                  PT_Entry.Present      := True;
                  PT_Entry.Read_Write   := True;
                  PT_Entry.U_S          := False;
                  PT_Entry.PWT          := False;
                  PT_Entry.PCD          := False;
                  PT_Entry.A            := False;

                  --  Shift the address right 12 bits to fit the 20bit format.
                  PT_Entry.Page_Address :=
                    Physical_Page_Address (Shift_Right (Current_Address, 12));

                  --  Since the memory addresses are all page aligned
                  --  we can just keep incrementing this for the purpose
                  --  of identity mapping.
                  Current_Address := Current_Address + 16#1000#;
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
end x86.Paging;
