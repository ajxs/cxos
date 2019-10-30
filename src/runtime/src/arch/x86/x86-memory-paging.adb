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
with x86.Memory.Map;

package body x86.Memory.Paging is
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Allocate_Page_Frame
   ----------------------------------------------------------------------------
   function Allocate_Page_Frame (
     Virtual_Address :     System.Address;
     Frame_Address   : out Page_Aligned_Address
   ) return Process_Result is
   begin
      pragma Unreferenced (Virtual_Address);
      pragma Unreferenced (Frame_Address);

      return Success;
   end Allocate_Page_Frame;

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
   --  Initialise_Kernel_Page_Directory
   ----------------------------------------------------------------------------
   procedure Initialise_Kernel_Page_Directory is
      use x86.Memory.Map;

      --  The result of the frame allocation operation.
      Allocate_Result : x86.Memory.Map.Process_Result;
   begin
      --  Allocate the Kernel page directory frame.
      --  This populates the Kernel page directory address with the address
      --  of the newly allocated frame.
      Allocate_Result := x86.Memory.Map.Allocate_Frame (
        Kernel_Page_Directory_Addr);
      if Allocate_Result /= Success then
         return;
      end if;

      Init_Directory :
         declare
            --  The Kernel Page Directory.
            Kernel_Page_Directory : Page_Directory
            with Import,
              Convention => Ada,
              Address    => Kernel_Page_Directory_Addr;

            --  Process result of internal processes.
            Result : Process_Result;
         begin
            --  Initialise the page directory.
            Init_Page_Dir :
               begin
                  Result := Initialise_Page_Directory (Kernel_Page_Directory);
                  if Result /= Success then
                     return;
                  end if;
               exception
                  when Constraint_Error =>
                     return;
               end Init_Page_Dir;

            --  Identity map the kernel.
            Identity_Map_Directory :
               declare
                  --  The current address being mapped.
                  Current_Addr : Integer_Address := 0;
               begin
                  for I in 0 .. 1023 loop
                     Result := Map_Page_Frame (Kernel_Page_Directory,
                       To_Address (Current_Addr), To_Address (Current_Addr));
                     if Result /= Success then
                        return;
                     end if;

                     --  Increment the counter by one page frame in size.
                     Current_Addr := Current_Addr + 16#1000#;
                  end loop;
               exception
                  when Constraint_Error =>
                     return;
               end Identity_Map_Directory;
         end Init_Directory;
   end Initialise_Kernel_Page_Directory;

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

   ----------------------------------------------------------------------------
   --  Map_Page_Frame
   ----------------------------------------------------------------------------
   function Map_Page_Frame (
     Directory     : in out Page_Directory;
     Physical_Addr :        System.Address;
     Virtual_Addr  :        System.Address
   ) return Process_Result is
      --  Index variables used when mapping  the page directory and table.
      Directory_Idx : Natural;
      Table_Idx     : Natural;
      --  Result variable for internal processes.
      Result        : Process_Result;
      --  Address variables used during the mapping process.
      Table_Addr    : System.Address;
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

      --  Get the indexes into the page directory and page table.
      Get_Indexes :
         begin
            Result := Get_Page_Directory_Index (Virtual_Addr, Directory_Idx);
            if Result /= Success then
               return Result;
            end if;

            Result := Get_Page_Table_Index (Virtual_Addr, Table_Idx);
            if Result /= Success then
               return Result;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Get_Indexes;

      --  Get the address of the page table.
      Get_Table_Address :
         declare
            use x86.Memory.Map;

            --  The process result of allocating a new page frame, if needed.
            Allocate_Result : x86.Memory.Map.Process_Result;
            --  The address of the newly allocated page frame, if applicable.
            Allocated_Addr  : System.Address;
         begin
            --  If there is no entry currently at this index in the page
            --  directory, allocate a new frame for to hold this page table,
            --  then allocate and initialise the new page table.
            if not Directory (Directory_Idx).Present then
               --  Allocate a page frame for the new page table.
               Allocate_Result := x86.Memory.Map.Allocate_Frame (
                 Allocated_Addr);
               if Allocate_Result /= Success then
                  return Invalid_Value;
               end if;

               --  Initialise the newly allocated page table.
               Init_Table :
                  declare
                     --  The process result of the initialisation.
                     Init_Result : Process_Result;
                     --  The newly allocated page table.
                     Table       : Page_Table
                     with Import,
                       Convention => Ada,
                       Address    => Allocated_Addr;
                  begin
                     --  Initialise the new page table.
                     Init_Result := Initialise_Page_Table (Table);
                     if Init_Result /= Success then
                        return Init_Result;
                     end if;
                  end Init_Table;

               --  Set the address at the applicable index into the page
               --  directory to point to this page table.
               Directory (Directory_Idx).Table_Address :=
                 Convert_To_Page_Aligned_Address (Allocated_Addr);
               Directory (Directory_Idx).Present := True;
            end if;

            Table_Addr := Convert_To_System_Address (
              Directory (Directory_Idx).Table_Address);
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Get_Table_Address;

      Map_Entry :
         declare
            --  The page table to map the entry in.
            Table : Page_Table
            with Import,
              Convention => Ada,
              Address    => Table_Addr;
         begin
            Table (Table_Idx).Page_Address :=
              Convert_To_Page_Aligned_Address (Physical_Addr);
            Table (Table_Idx).Present      := True;
         exception
            when Constraint_Error =>
               return Invalid_Table_Index;
         end Map_Entry;

      return Success;
   end Map_Page_Frame;
end x86.Memory.Paging;
