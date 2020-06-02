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

with Ada.Characters.Latin_1;
with Cxos.Debug;
with Cxos.Memory;
with Cxos.Memory.Map;
with Multiboot;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;

package body Cxos.Boot.Multiboot_Init is
   package Chars renames Ada.Characters.Latin_1;
   procedure Debug_Print (Data : String) renames Cxos.Debug.Put_String;

   ----------------------------------------------------------------------------
   --  Clear_Multiboot_Reserved_Data
   ----------------------------------------------------------------------------
   function Clear_Multiboot_Reserved_Data return Process_Result is
      use Cxos.Memory;
      use Cxos.Memory.Map;

      --  Marker to the start of the multiboot reserved memory area.
      Multiboot_Reserved_Memory_Start : constant System.Address
      with Import,
        Convention    => Ada,
        External_Name => "multiboot_reserved_start";

      --  Marker to the end of the multiboot reserved memory area.
      Multiboot_Reserved_Memory_End   : constant System.Address
      with Import,
        Convention    => Ada,
        External_Name => "multiboot_reserved_end";

      --  The number of 4kb memory frames contained within the multiboot
      --  reserved memory area.
      Frame_Count : Unsigned_32;

      --  The result of internal processes.
      Result : Cxos.Memory.Process_Result;
   begin
      Debug_Print ("Freeing multiboot memory" & Chars.LF);

      --  Get the number of memory frames within the multiboot reserved area.
      Frame_Count := Unsigned_32 (
        Multiboot_Reserved_Memory_End'Address -
        Multiboot_Reserved_Memory_Start'Address) / 16#1000#;

      --  Mark the reserved memory area as unallocated.
      Cxos.Memory.Map.Mark_Memory_Range (
        Multiboot_Reserved_Memory_Start, Frame_Count, Unallocated, Result);
      if Result /= Success then
         Debug_Print ("Error marking multiboot memory" & Chars.LF);
         return Unhandled_Exception;
      end if;

      Debug_Print ("Finished freeing multiboot memory" & Chars.LF);

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Clear_Multiboot_Reserved_Data;

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Drive_Map
   ----------------------------------------------------------------------------
   function Parse_Multiboot_Drive_Map (
     Drive_Map_Addr   : System.Address;
     Drive_Map_Length : Unsigned_32
   ) return Process_Result is
      --  Pointer conversions package.
      package Drive_Entry_Ptr is new
        System.Address_To_Access_Conversions (Multiboot.Multiboot_Drive_Entry);

      --  The total number of bytes read in the Multiboot drive map.
      Bytes_Read : Unsigned_32    := 0;
      --  The address of the current drive map structure.
      Curr_Addr  : System.Address := Drive_Map_Addr;
      --  A pointer to the current drive map structure.
      Curr_Entry : Drive_Entry_Ptr.Object_Pointer :=
        Drive_Entry_Ptr.To_Pointer (Curr_Addr);
   begin
      while Bytes_Read < Drive_Map_Length loop
         --  Reset the current region pointer.
         Curr_Entry := Drive_Entry_Ptr.To_Pointer (Curr_Addr);

         --  Print information about the current drive entry.
         Print_Drive_Entry_Info :
            begin
               Debug_Print ("Parsing Drive Entry" & Chars.LF);
            end Print_Drive_Entry_Info;

         --  Increment the entry.
         Increment_Pointer :
            begin
               --  The 'Size' value is not inclusive of the size variable
               --  itself. It refers to the size of the internal structure.
               Curr_Addr := To_Address (To_Integer (Curr_Addr) +
                 Integer_Address (4 + Curr_Entry.all.Size));

               Bytes_Read := Bytes_Read + 4 + Curr_Entry.all.Size;
            exception
               when Constraint_Error =>
                  return Unhandled_Exception;
            end Increment_Pointer;
      end loop;

      return Success;
   end Parse_Multiboot_Drive_Map;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Info
   function Parse_Multiboot_Info return Process_Result is
      --  The multiboot drive map info struct.
      Drive_Info : constant Multiboot_Section_Info
      with Import,
        Convention    => Assembler,
        External_Name => "multiboot_drive_info",
        Volatile;

      --  The multiboot memory map info struct.
      Mmap_Info : constant Multiboot_Section_Info
      with Import,
        Convention    => Assembler,
        External_Name => "multiboot_mmap_info",
        Volatile;

      --  The result of internal processes.
      Result : Process_Result;
   begin
      --  Check whether we have a valid Multiboot memory map.
      if Mmap_Info.Section_Present then
         Debug_Print (
           "Multiboot memory map present" & Chars.LF &
           "Parsing Multiboot memory map" & Chars.LF);

         --  Parse the Multiboot provided memory map to mark memory
         --  regions that are free to use.
         Result := Parse_Multiboot_Memory_Map (Mmap_Info.Section_Addr,
           Mmap_Info.Section_Length);
         if Result /= Success then
            Debug_Print ("Error parsing memory map" & Chars.LF);
            return Unhandled_Exception;
         end if;

         Debug_Print ("Finished parsing memory map" & Chars.LF);
      else
         Debug_Print (
           "Multiboot memory map not present" & Chars.LF);
      end if;

      --  As per Multiboot spec, the drive map section length can be valid
      --  with a length of 0.
      if Drive_Info.Section_Present and Drive_Info.Section_Length > 0 then
         Debug_Print (
           "Multiboot drives map present" & Chars.LF &
           "Parsing drive entries" & Chars.LF);

         Result := Parse_Multiboot_Drive_Map (Drive_Info.Section_Addr,
           Drive_Info.Section_Length);
         if Result /= Success then
            Debug_Print ("Error parsing drive map" & Chars.LF);
            return Unhandled_Exception;
         end if;

         Debug_Print ("Finished parsing drive map" & Chars.LF);
      else
         Debug_Print (
           "Multiboot drives map not present" & Chars.LF);
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Parse_Multiboot_Info;

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Memory_Map
   ----------------------------------------------------------------------------
   function Parse_Multiboot_Memory_Map (
     Memory_Map_Addr   : System.Address;
     Memory_Map_Length : Unsigned_32
   ) return Process_Result is
      use Cxos.Memory;
      use Cxos.Memory.Map;

      package Mmap_Region_Ptr is new
        System.Address_To_Access_Conversions (Multiboot.Multiboot_Mmap_Region);

      --  The total number of bytes read in the Multiboot mmap region.
      Bytes_Read  : Unsigned_32    := 0;
      --  The address of the current mmap region structure.
      Curr_Addr   : System.Address := Memory_Map_Addr;
      --  A pointer to the current mmap region structure.
      Curr_Region : Mmap_Region_Ptr.Object_Pointer :=
        Mmap_Region_Ptr.To_Pointer (Curr_Addr);
   begin
      while Bytes_Read < Memory_Map_Length loop
         --  Reset the current region pointer.
         Curr_Region := Mmap_Region_Ptr.To_Pointer (Curr_Addr);

         --  Print information about the current mmap region.
         Print_Memory_Region_Info :
            begin
               Debug_Print ("------------------------" & Chars.LF);
               Debug_Print ("Mmap region:" & Chars.LF);
               Debug_Print ("  Type:    ");

               case Curr_Region.all.Memory_Type is
                  when 1 =>
                     Debug_Print ("Free RAM" & Chars.LF);
                  when 3 =>
                     Debug_Print ("ACPI" & Chars.LF);
                  when 4 =>
                     Debug_Print (
                       "Reserved for hibernation" & Chars.LF);
                  when 5 =>
                     Debug_Print ("Defective" & Chars.LF);
                  when others =>
                     Debug_Print ("Reserved" & Chars.LF);
               end case;

               Debug_Print ("  Base:   " &
                 Unsigned_32 (Curr_Region.all.Base and 16#FFFF_FFFF#)'Image &
                 Chars.LF);
               Debug_Print ("  Length: " &
                 Unsigned_32 (Curr_Region.all.Length and 16#FFFF_FFFF#)'Image &
                 Chars.LF);
            exception
               when Constraint_Error =>
                  return Unhandled_Exception;
            end Print_Memory_Region_Info;

         --  Mark free memory in the kernel memory map.
         Mark_Free_Memory :
            declare
               --  The result of the frame status set process.
               Result : Cxos.Memory.Process_Result := Success;
            begin
               --  If the memory region is marked as free, set the status
               --  accordingly in the memory map.
               case Curr_Region.all.Memory_Type is
                  when 1 =>
                     Cxos.Memory.Map.Mark_Memory_Range (
                       To_Address (Integer_Address (Curr_Region.all.Base)),
                       Unsigned_32 (Curr_Region.all.Length),
                       Unallocated,
                       Result);
                     if Result /= Success then
                        Debug_Print (
                          "Error setting frame status" & Chars.LF);
                        return Unhandled_Exception;
                     end if;
                  when others =>
                     null;
               end case;
            exception
               when Constraint_Error =>
                  Debug_Print ("Error marking free memory" & Chars.LF);

                  return Unhandled_Exception;
            end Mark_Free_Memory;

         Increment_Pointer :
            begin
               --  The 'Size' value is not inclusive of the size variable
               --  itself. It refers to the size of the internal structure.
               Curr_Addr := To_Address (To_Integer (Curr_Addr) +
                 Integer_Address (4 + Curr_Region.all.Size));

               Bytes_Read := Bytes_Read + 4 + Curr_Region.all.Size;
            exception
               when Constraint_Error =>
                  return Unhandled_Exception;
            end Increment_Pointer;
      end loop;

      return Success;
   end Parse_Multiboot_Memory_Map;
end Cxos.Boot.Multiboot_Init;
