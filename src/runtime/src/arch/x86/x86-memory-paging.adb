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

package body x86.Memory.Paging is
   use Interfaces;
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Check_Address_Page_Aligned
   ----------------------------------------------------------------------------
   function Check_Address_Page_Aligned (
     Addr : System.Address
   ) return Boolean is
   begin
      return (To_Integer (Addr) and 16#FFF#) = 0;
   end Check_Address_Page_Aligned;

   ----------------------------------------------------------------------------
   --  Convert_To_Page_Aligned_Address
   --
   --  Implementation Notes:
   --   - Converts the address to a 32 bit unsigned integer in order to
   --     properly truncate the value to the 4kb aligned 20-bit value.
   ----------------------------------------------------------------------------
   function Convert_To_Page_Aligned_Address (
     Addr : System.Address
   ) return Page_Aligned_Address is
      Address_As_Unsigned : Unsigned_32;
   begin
      Address_As_Unsigned := Unsigned_32 (To_Integer (Addr));
      Address_As_Unsigned := Address_As_Unsigned and 16#FFFFF000#;
      Address_As_Unsigned := Shift_Right (Address_As_Unsigned, 12);

      return Page_Aligned_Address (Address_As_Unsigned);
   exception
      when Constraint_Error =>
         return 0;
   end Convert_To_Page_Aligned_Address;

   ----------------------------------------------------------------------------
   --  Convert_To_System_Address
   ----------------------------------------------------------------------------
   function Convert_To_System_Address (
     Addr : Page_Aligned_Address
   ) return System.Address is
      Address_As_Unsigned : Unsigned_32;
   begin
      Address_As_Unsigned := Unsigned_32 (Addr);
      Address_As_Unsigned := Shift_Left (Address_As_Unsigned, 12);

      return To_Address (Integer_Address (Address_As_Unsigned));
   exception
      when Constraint_Error =>
         return System.Null_Address;
   end Convert_To_System_Address;

   ----------------------------------------------------------------------------
   --  Get_Page_Address
   ----------------------------------------------------------------------------
   function Get_Page_Address (
     Table_Index :     Natural;
     Page_Index  :     Natural;
     Addr        : out System.Address
   ) return Process_Result is
      --  The base address of the page table.
      Table_Addr : Integer_Address;

      --  The address of the page frame itself.
      Page_Addr  : Integer_Address;
   begin
      --  Compute the address of this page table in virtual memory.
      Table_Addr := Integer_Address (Table_Index * 16#400000#);

      --  Compute the address of the page frame.
      Page_Addr := Integer_Address (Page_Index * 16#1000#);

      --  The address of the page frame itself is the sum of the table
      --  address base and the page address.
      Addr := To_Address (Table_Addr + Page_Addr);

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Get_Page_Address;

   ----------------------------------------------------------------------------
   --  Get_Page_Directory_Index
   ----------------------------------------------------------------------------
   function Get_Page_Directory_Index (
     Addr  :     System.Address;
     Index : out Natural
   ) return Process_Result is
      Addr_As_Uint : Unsigned_32;
   begin
      --  Ensure that the provided address is 4K aligned.
      Check_Address :
         begin
            if not Check_Address_Page_Aligned (Addr) then
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
     Addr  :     System.Address;
     Index : out Natural
   ) return Process_Result is
      Addr_As_Uint : Unsigned_32;
   begin
      --  Ensure that the provided address is 4K aligned.
      Check_Address :
         begin
            if not Check_Address_Page_Aligned (Addr) then
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
   --  Initialise_Page_Directory
   ----------------------------------------------------------------------------
   function Initialise_Page_Directory (
     Page_Dir : in out Page_Directory
   ) return Process_Result is
   begin
      --  Iterate over all 1024 directory entries.
      for Idx in 0 .. 1023 loop
         --  Initialise the individual entry.
         Initialise_Entry :
            begin
               Page_Dir (Idx).Present       := False;
               Page_Dir (Idx).Read_Write    := True;
               Page_Dir (Idx).U_S           := False;
               Page_Dir (Idx).PWT           := False;
               Page_Dir (Idx).PCD           := False;
               Page_Dir (Idx).A             := False;
               Page_Dir (Idx).PS            := False;
               Page_Dir (Idx).G             := False;
               Page_Dir (Idx).Table_Address :=
                 Convert_To_Page_Aligned_Address (System.Null_Address);
            exception
               when Constraint_Error =>
                  return Invalid_Value;
            end Initialise_Entry;
      end loop;

      return Success;
   end Initialise_Page_Directory;

   ----------------------------------------------------------------------------
   --  Initialise_Page_Table
   ----------------------------------------------------------------------------
   function Initialise_Page_Table (
     Table : in out Page_Table
   ) return Process_Result is
   begin
      for Idx in 0 .. 1023 loop
         Initialise_Entry :
            begin
               Table (Idx).Present      := False;
               Table (Idx).Read_Write   := True;
               Table (Idx).U_S          := False;
               Table (Idx).PWT          := False;
               Table (Idx).PCD          := False;
               Table (Idx).A            := False;
               Table (Idx).Page_Address :=
                 Convert_To_Page_Aligned_Address (System.Null_Address);
            exception
               when Constraint_Error =>
                  return Invalid_Value;
            end Initialise_Entry;
      end loop;

      return Success;
   end Initialise_Page_Table;

end x86.Memory.Paging;
