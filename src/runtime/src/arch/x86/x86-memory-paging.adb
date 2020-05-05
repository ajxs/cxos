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

with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package body x86.Memory.Paging is
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
     Table_Index : Paging_Index;
     Page_Index  : Paging_Index
   ) return System.Address is
      --  The base address of the page table.
      Table_Addr : Integer_Address;

      --  The address of the page frame itself.
      Page_Addr  : Integer_Address;
   begin
      if not Table_Index'Valid or not Page_Index'Valid then
         return System.Null_Address;
      end if;

      --  Compute the address of this page table in virtual memory.
      Table_Addr := Integer_Address (Table_Index * 16#400000#);

      --  Compute the address of the page frame.
      Page_Addr := Integer_Address (Page_Index * 16#1000#);

      --  The address of the page frame itself is the sum of the table
      --  address base and the page address.
      return To_Address (Table_Addr + Page_Addr);
   exception
      when Constraint_Error =>
         return System.Null_Address;
   end Get_Page_Address;

   ----------------------------------------------------------------------------
   --  Get_Page_Directory_Index
   ----------------------------------------------------------------------------
   function Get_Page_Directory_Index (
     Addr : System.Address
   ) return Paging_Index is
      Addr_As_Uint : Unsigned_32;
   begin
      --  Return only highest-order 10 bits.
      Addr_As_Uint := Unsigned_32 (To_Integer (Addr));
      Addr_As_Uint := Shift_Right (Addr_As_Uint, 22);

      return Paging_Index (Addr_As_Uint);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Page_Directory_Index;

   ----------------------------------------------------------------------------
   --  Get_Page_Table_Index
   ----------------------------------------------------------------------------
   function Get_Page_Table_Index (
     Addr : System.Address
   ) return Paging_Index is
      Addr_As_Uint : Unsigned_32;
   begin
      --  Return only the middle 10 bits.
      Addr_As_Uint := Unsigned_32 (To_Integer (Addr));
      Addr_As_Uint := Shift_Right (Addr_As_Uint, 12) and 16#03FF#;

      return Paging_Index (Addr_As_Uint);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Page_Table_Index;

end x86.Memory.Paging;
