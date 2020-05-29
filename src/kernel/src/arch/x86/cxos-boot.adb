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
with x86.IDT;
with x86.Interrupts;
with x86.GDT;
with x86.PIC;
with x86.Serial;

package body Cxos.Boot is
   package Chars renames Ada.Characters.Latin_1;
   procedure Debug_Print (Data : String) renames Cxos.Debug.Put_String;

   ----------------------------------------------------------------------------
   --  Initialise_Kernel
   ----------------------------------------------------------------------------
   procedure Initialise_Kernel is
   begin
      --  Initialise the COM1 Serial port, which will be used for all
      --  subsequent debugging output.
      x86.Serial.Initialise (x86.Serial.COM1, 38400);
      x86.Serial.Put_String (x86.Serial.COM1,
        "COM1 initialised" & Chars.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising PIC" & Chars.LF);
      x86.PIC.Initialise;

      --  Clear interrupts.
      x86.Interrupts.Set_Interrupt_Flag (False);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising GDT" & Chars.LF);
      x86.GDT.Initialise;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Finished initialising GDT" & Chars.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising IDT" & Chars.LF);
      x86.IDT.Initialise;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Finished initialising IDT" & Chars.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Loading IDT" & Chars.LF);
      x86.IDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1, "Flushing GDT" & Chars.LF);
      x86.GDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Jumping to protected mode" & Chars.LF);
      Protected_Mode_Init;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Protected mode entered" & Chars.LF);

      --  Enable interrupts.
      x86.Interrupts.Set_Interrupt_Flag (True);

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
            Debug_Print ("Initialising Interrupts" & Chars.LF);
            Init_Result := Cxos.Interrupts.Initialise;
            if Init_Result /= Success then
               Debug_Print ("Error initialising Interrupts" & Chars.LF);
               return;
            end if;
            Debug_Print ("Finished initialising interrupts" & Chars.LF);

            Debug_Print ("Initialising CPU exceptions" & Chars.LF);
            Cxos.Exceptions.Initialise;
            Debug_Print ("Finished initialising CPU exceptions" & Chars.LF);
         end Initialise_Interrupts;

      --  Initialise system timer and PIT before re-enabling interrupt
      --  generation.
      Initialise_Timers :
         begin
            Debug_Print ("Initialising system timer" & Chars.LF);
            Cxos.Time_Keeping.Initialise;
            Debug_Print (
              "Finished initialising system timer" & Chars.LF);

            Debug_Print ("Initialising PIT" & Chars.LF);
            Cxos.PIT.Initialise;
            Debug_Print ("Finished initialising PIT" & Chars.LF);
         end Initialise_Timers;

      --  Initialise the kernel memory map.
      Init_Memory_Map :
         begin
            --  Initialise the system memory map.
            Debug_Print ("Initialising Memory Map" & Chars.LF);
            Cxos.Memory.Map.Initialise;
            Debug_Print ("Finished Memory Map init" & Chars.LF);
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
               Debug_Print (
                 "Detected valid Multiboot magic number" & Chars.LF &
                 "Parsing Multiboot info" & Chars.LF);

               Init_Result := Cxos.Boot.Multiboot_Init.Parse_Multiboot_Info;
               if Init_Result /= Success then
                  Debug_Print ("Error parsing multiboot info" & Chars.LF);
                  return;
               end if;

               Init_Result :=
                 Cxos.Boot.Multiboot_Init.Clear_Multiboot_Reserved_Data;
               if Init_Result /= Success then
                  Debug_Print ("Error freeing multiboot memory" & Chars.LF);
                  return;
               end if;

               Debug_Print ("Finished parsing multiboot info" & Chars.LF);
            else
               Debug_Print (
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
            Debug_Print ("Marking kernel memory" & Chars.LF);
            Init_Result := Cxos.Memory.Mark_Kernel_Memory;
            if Init_Result /= Success then
               Debug_Print ("Error marking kernel memory" & Chars.LF);
               return;
            end if;
            Debug_Print ("Finished marking kernel memory" & Chars.LF);
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
