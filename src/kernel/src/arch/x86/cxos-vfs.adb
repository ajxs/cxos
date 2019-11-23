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

with Cxos.Serial;
with Multiboot;

package body Cxos.VFS is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   function Initialise return Kernel_Process_Result is
      use Multiboot;
      use System.Storage_Elements;

      --  Multiboot info struct address.
      Boot_Info_Address : constant System.Address
      with Import,
        Convention    => Assembler,
        External_Name => "multiboot_struct",
        Volatile;
      --  Create multiboot info structure overlaid at boot info address.
      Boot_Info         : constant Multiboot_Info
      with Import,
        Convention    => Assembler,
        Address       => Boot_Info_Address,
        Volatile;
   begin
      if Boot_Info.Flags.Drives_Fields_Valid then
         Cxos.Serial.Put_String (
           "Multiboot Drives map present" & ASCII.LF &
           "Parsing Drive entries" & ASCII.LF);

         Parse_Multiboot_Drive_Map (
           To_Address (Integer_Address (Boot_Info.Drives_Addr)),
           Boot_Info.Drives_Length);
      end if;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Drive_Map
   ----------------------------------------------------------------------------
   function Parse_Multiboot_Drive_Map (
     Drive_Map_Addr   : System.Address;
     Drive_Map_Length : Unsigned_32
   ) return Kernel_Process_Result is
      use System.Storage_Elements;

      package Drive_Entry_Ptr is new
        System.Address_To_Access_Conversions (Multiboot.Multiboot_Drive_Entry);

      --  The total number of bytes read in the Multiboot drive map.
      Bytes_Read : Unsigned_32    := 0;
      --  The address of the current drive map structure.
      Curr_Addr  : System.Address := Drive_Map_Addr;
      --  A pointer to the current drive map structure.
      Curr_Entry : Mmap_Region_Ptr.Object_Pointer :=
        Drive_Entry_Ptr.To_Pointer (Curr_Addr);
   begin
      while Bytes_Read < Drive_Map_Length loop
         --  Reset the current region pointer.
         Curr_Region := Drive_Entry_Ptr.To_Pointer (Curr_Addr);

         --  Print information about the current drive entry.
         Print_Drive_Entry_Info :
            begin
               Cxos.Serial.Put_String ("Parsing Drive Entry" & ASCII.LF);
            end Print_Drive_Entry_Info;

         Increment_Pointer :
            begin
               --  The 'Size' value is not inclusive of the size variable
               --  itself. It refers to the size of the internal structure.
               Curr_Addr := To_Address (To_Integer (Curr_Addr) +
                 Integer_Address (4 + Curr_Region.all.Size));

               Bytes_Read := Bytes_Read + 4 + Curr_Region.all.Size;
            exception
               when Constraint_Error =>
                  return;
            end Increment_Pointer;
      end loop;

      return Success;
   end Parse_Multiboot_Drive_Map;
end Cxos.VFS;


