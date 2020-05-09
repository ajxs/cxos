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
with Cxos.Boot.Multiboot_Init;
with Cxos.Debug;
with Cxos.Devices;
with Cxos.Devices.Graphics.Vga;
with Cxos.Exceptions;
with Cxos.Interrupts;
with Cxos.Memory;
with Cxos.Memory.Map;
with Cxos.PIT;
with Cxos.Tasking;
with Cxos.Time_Keeping;
with Interfaces; use Interfaces;
with Multiboot;
with x86.Vga;

package body Cxos.Boot is
   package Chars renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------------
   --  Initialise_Kernel
   ----------------------------------------------------------------------------
   procedure Initialise_Kernel is
   begin
      --  Initialise VGA graphics buffer.
      Initialise_Vga :
         declare
            use x86.Vga;
         begin
            Cxos.Devices.Graphics.Vga.Clear (Black);
            Cxos.Devices.Graphics.Vga.Put_String (0, 0, Light_Green, Black,
              "VGA Text Mode Initialised");
         end Initialise_Vga;

      --  Initialise system interrupts and processor exceptions.
      Initialise_Interrupts :
         declare
            use Cxos.Interrupts;

            --  The result of the internal initialisation process.
            Init_Result : Cxos.Interrupts.Process_Result;
         begin
            Cxos.Debug.Put_String ("Initialising Interrupts" & Chars.LF);
            Init_Result := Cxos.Interrupts.Initialise;
            if Init_Result /= Success then
               Cxos.Debug.Put_String ("Error initialising Interrupts"
                 & Chars.LF);
               return;
            end if;
            Cxos.Debug.Put_String ("Finished initialising interrupts" &
              Chars.LF);

            Cxos.Debug.Put_String ("Initialising CPU exceptions" & Chars.LF);
            Cxos.Exceptions.Initialise;
            Cxos.Debug.Put_String ("Finished initialising CPU exceptions" &
              Chars.LF);
         end Initialise_Interrupts;

      --  Initialise system timer and PIT before re-enabling interrupt
      --  generation.
      Initialise_Timers :
         begin
            Cxos.Debug.Put_String ("Initialising system timer" & Chars.LF);
            Cxos.Time_Keeping.Initialise;
            Cxos.Debug.Put_String (
              "Finished initialising system timer" & Chars.LF);

            Cxos.Debug.Put_String ("Initialising PIT" & Chars.LF);
            Cxos.PIT.Initialise;
            Cxos.Debug.Put_String ("Finished initialising PIT" & Chars.LF);
         end Initialise_Timers;

      --  Initialise the kernel memory map.
      Init_Memory_Map :
         begin
            --  Initialise the system memory map.
            Cxos.Debug.Put_String ("Initialising Memory Map" & Chars.LF);
            Cxos.Memory.Map.Initialise;
            Cxos.Debug.Put_String ("Finished Memory Map init" & Chars.LF);
         end Init_Memory_Map;

      --  Read the multiboot info structures.
      --  If these are present, additional kernel init procedures will
      --  be run, such as marking memory according to the multiboot
      --  memory map.
      Read_Multiboot_Info :
         declare
            use Cxos.Boot.Multiboot_Init;
            use Multiboot;

            --  The Multiboot magic number.
            Magic_Number : Multiboot_Magic_Number
            with Import,
              Convention    => Assembler,
              External_Name => "multiboot_magic";

            --  The result of the initialisation process.
            Init_Result : Cxos.Boot.Multiboot_Init.Process_Result;
         begin
            --  Check whether we were booted by a Multiboot compatible
            --  bootloader.
            if Magic_Number = VALID_MAGIC_NUMBER then
               Cxos.Debug.Put_String (
                 "Detected valid Multiboot magic number" & Chars.LF &
                 "Parsing Multiboot info" & Chars.LF);

               Init_Result := Cxos.Boot.Multiboot_Init.Parse_Multiboot_Info;
               if Init_Result /= Success then
                  Cxos.Debug.Put_String ("Error parsing multiboot info"
                    & Chars.LF);
                  return;
               end if;

               Init_Result :=
                 Cxos.Boot.Multiboot_Init.Clear_Multiboot_Reserved_Data;
               if Init_Result /= Success then
                  Cxos.Debug.Put_String ("Error freeing multiboot memory"
                    & Chars.LF);
                  return;
               end if;

               Cxos.Debug.Put_String (
                 "Finished parsing multiboot info" & Chars.LF);
            else
               Cxos.Debug.Put_String (
                 "Unable to detect valid Multiboot magic number" & Chars.LF);

               --  Exit here in the instance that we can't find a valid
               --  Multiboot identity.
               return;
            end if;
         end Read_Multiboot_Info;

      --  Mark the memory used by the kernel as non-present.
      Mark_Kernel_Memory :
         declare
            use Cxos.Memory;

            --  The result of the process.
            Init_Result : Cxos.Memory.Process_Result;
         begin
            Cxos.Debug.Put_String ("Marking kernel memory" & Chars.LF);
            Init_Result := Cxos.Memory.Mark_Kernel_Memory;
            if Init_Result /= Success then
               Cxos.Debug.Put_String ("Error marking kernel memory"
                 & Chars.LF);
               return;
            end if;
            Cxos.Debug.Put_String ("Finished marking kernel memory"
              & Chars.LF);
         end Mark_Kernel_Memory;

      --  Initialise peripheral devices.
      Initialise_Devices :
         begin
            Cxos.Devices.Initialise;
         end Initialise_Devices;

      Initialise_Tasking :
         begin
            Cxos.Tasking.Initialise;
         end Initialise_Tasking;
   exception
      when Constraint_Error =>
         return;
   end Initialise_Kernel;

end Cxos.Boot;
