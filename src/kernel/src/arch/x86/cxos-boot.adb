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
with Cxos.Devices.Serial;
with Cxos.Error_Handling;
with Cxos.Exceptions;
with Cxos.GDT;
with Cxos.IDT;
with Cxos.Interrupts;
with Cxos.Memory;
with Cxos.Memory.Map;
with Cxos.PIC;
with Cxos.PIT;
with Cxos.Time_Keeping;
with Interfaces; use Interfaces;
with Multiboot;
with System.Storage_Elements; use System.Storage_Elements;
with x86.Vga;
with x86.Serial;

package body Cxos.Boot is
   package Chars renames Ada.Characters.Latin_1;
   procedure Debug_Print (Data : String) renames Cxos.Debug.Put_String;
   --  Error logging function shorthand.
   procedure Log_Error (Message : String)
     renames Cxos.Error_Handling.Log_Kernel_Error;

   ----------------------------------------------------------------------------
   --  Initialise_Kernel
   ----------------------------------------------------------------------------
   procedure Initialise_Kernel is
      --  The result of subprocedure calls.
      Status : Program_Status := Unset;
   begin
      --  Initialise the COM1 Serial port, which will be used for all
      --  subsequent debugging output.
      Cxos.Devices.Serial.Initialise (x86.Serial.COM1, 38400);
      Debug_Print ("COM1 initialised" & Chars.LF);

      Debug_Print ("Initialising PIC" & Chars.LF);
      Cxos.PIC.Initialise;

      --  Clear interrupts.
      Cxos.Interrupts.Set_Interrupt_Flag (False);

      Debug_Print ("Initialising GDT" & Chars.LF);
      Cxos.GDT.Initialise;
      Debug_Print ("Finished initialising GDT" & Chars.LF);

      Debug_Print ("Initialising IDT" & Chars.LF);
      Cxos.IDT.Initialise;
      Debug_Print ("Finished initialising IDT" & Chars.LF);

      Debug_Print ("Loading IDT" & Chars.LF);
      Cxos.IDT.Finalise;

      Debug_Print ("Flushing GDT" & Chars.LF);
      Cxos.GDT.Finalise;

      Debug_Print ("Jumping to protected mode" & Chars.LF);
      Protected_Mode_Init;
      Debug_Print ("Protected mode entered" & Chars.LF);

      --  Enable interrupts.
      Cxos.Interrupts.Set_Interrupt_Flag (True);

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
               Log_Error ("Error initialising Interrupts" & Chars.LF);
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
                  Log_Error ("Error parsing multiboot info" & Chars.LF);
                  return;
               end if;

               Init_Result :=
                 Cxos.Boot.Multiboot_Init.Clear_Multiboot_Reserved_Data;
               if Init_Result /= Success then
                  Log_Error ("Error freeing multiboot memory" & Chars.LF);
                  return;
               end if;

               Debug_Print ("Finished parsing multiboot info" & Chars.LF);
            else
               Log_Error (
                 "Unable to detect valid Multiboot magic number" & Chars.LF);

               --  Exit here in the instance that we can't find a valid
               --  Multiboot identity.
               return;
            end if;
         end Read_Multiboot_Info;

      --  Mark the memory used by the kernel as non-present.
      Debug_Print ("Marking kernel memory" & Chars.LF);
      Mark_Kernel_Memory (Status);
      if Status /= Success then
         Log_Error ("Error marking kernel memory" & Chars.LF);
         return;
      end if;
      Debug_Print ("Finished marking kernel memory" & Chars.LF);

      --  Initialise peripheral devices.
      Initialise_Devices :
         begin
            Cxos.Devices.Initialise;
         end Initialise_Devices;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint violation during kernel init" & Chars.LF);
         return;
   end Initialise_Kernel;

   ----------------------------------------------------------------------------
   --  Mark_Kernel_Memory
   --
   --  Implementation Notes:
   --    - Marks the kernel's physical memory as being used.
   ----------------------------------------------------------------------------
   procedure Mark_Kernel_Memory (Status : out Program_Status) is
      use Cxos.Memory;
      use Cxos.Memory.Map;

      --  The length of the kernel segments in bytes.
      Kernel_Length : Unsigned_32 := 0;

      --  The result of the frame status set process.
      Result : Process_Result := Unset;

      --  The start address of the kernel code.
      Kernel_Start     : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_Name => "kernel_start";
      --  The end address of the kernel code.
      Kernel_End       : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_Name => "kernel_end";
      --  The physical starting address of the kernel.
      Kernel_Physical_Start : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_name => "KERNEL_PHYS_START";
   begin
      Kernel_Length := Unsigned_32 (Kernel_End'Address - Kernel_Start'Address);

      Mark_Memory_Range (Kernel_Physical_Start'Address,
        Kernel_Length, Allocated, Result);
      if Result /= Success then
         Debug_Print ("Error marking kernel memory" & Chars.LF);

         Status := Unhandled_Exception;
         return;
      end if;

      Status := Success;
   exception
      when Constraint_Error =>
         Status := Unhandled_Exception;
   end Mark_Kernel_Memory;

end Cxos.Boot;
