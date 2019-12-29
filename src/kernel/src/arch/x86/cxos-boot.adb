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

with Cxos.Exceptions;
with Cxos.Interrupts;
with Cxos.Memory;
with Cxos.Memory.Map;
with Cxos.Multiboot_Init;
with Cxos.PCI;
with Cxos.PIT;
with Cxos.Process;
with Cxos.Serial;
with Cxos.Time_Keeping;
with Interfaces;
with Multiboot;

package body Cxos.Boot is
   use Interfaces;

   ----------------------------------------------------------------------------
   --  Initialise_Kernel
   ----------------------------------------------------------------------------
   procedure Initialise_Kernel is
   begin
      --  Initialise system interrupts and processor exceptions.
      Initialise_Interrupts :
         declare
            use Cxos.Interrupts;

            --  The result of the internal initialisation process.
            Init_Result : Cxos.Interrupts.Process_Result;
         begin
            Cxos.Serial.Put_String ("Initialising Interrupts" & ASCII.LF);
            Init_Result := Cxos.Interrupts.Initialise;
            if Init_Result /= Success then
               Cxos.Serial.Put_String ("Error initialising Interrupts"
                 & ASCII.LF);
               return;
            end if;
            Cxos.Serial.Put_String ("Finished initialising interrupts" &
              ASCII.LF);

            Cxos.Serial.Put_String ("Initialising CPU exceptions" & ASCII.LF);
            Cxos.Exceptions.Initialise;
            Cxos.Serial.Put_String ("Finished initialising CPU exceptions" &
              ASCII.LF);
         end Initialise_Interrupts;

      --  Initialise system timer and PIT before re-enabling interrupt
      --  generation.
      Initialise_Timers :
         begin
            Cxos.Serial.Put_String ("Initialising system timer" & ASCII.LF);
            Cxos.Time_Keeping.Initialise;
            Cxos.Serial.Put_String (
              "Finished initialising system timer" & ASCII.LF);

            Cxos.Serial.Put_String ("Initialising PIT" & ASCII.LF);
            Cxos.PIT.Initialise;
            Cxos.Serial.Put_String ("Finished initialising PIT" & ASCII.LF);
         end Initialise_Timers;

      --  Initialise the kernel memory map.
      Init_Memory_Map :
         begin
            --  Initialise the system memory map.
            Cxos.Serial.Put_String ("Initialising Memory Map" & ASCII.LF);
            Cxos.Memory.Map.Initialise;
            Cxos.Serial.Put_String ("Finished Memory Map init" & ASCII.LF);
         end Init_Memory_Map;

      --  Read the multiboot info structures.
      --  If these are present, additional kernel init procedures will
      --  be run, such as marking memory according to the multiboot
      --  memory map.
      Read_Multiboot_Info :
         declare
            use Cxos.Multiboot_Init;
            use Multiboot;

            --  The Multiboot magic number.
            Magic_Number : Multiboot_Magic_Number
            with Import,
              Convention    => Assembler,
              External_Name => "multiboot_magic";

            --  The result of the initialisation process.
            Init_Result : Cxos.Multiboot_Init.Process_Result;
         begin
            --  Check whether we were booted by a Multiboot compatible
            --  bootloader.
            if Magic_Number = VALID_MAGIC_NUMBER then
               Cxos.Serial.Put_String (
                 "Detected valid Multiboot magic number" & ASCII.LF &
                 "Parsing Multiboot info" & ASCII.LF);

               Init_Result := Cxos.Multiboot_Init.Parse_Multiboot_Info;
               if Init_Result /= Success then
                  Cxos.Serial.Put_String ("Error parsing multiboot info"
                    & ASCII.LF);
                  return;
               end if;

               Cxos.Serial.Put_String (
                 "Finished parsing multiboot info" & ASCII.LF);
            else
               Cxos.Serial.Put_String (
                 "Unable to detect valid Multiboot magic number" & ASCII.LF);
            end if;
         end Read_Multiboot_Info;

      --  Mark the memory used by the kernel as non-present.
      Mark_Kernel_Memory :
         declare
            use Cxos.Memory;

            --  The result of the process.
            Init_Result : Cxos.Memory.Process_Result;
         begin
            Cxos.Serial.Put_String ("Marking kernel memory" & ASCII.LF);
            Init_Result := Cxos.Memory.Mark_Kernel_Memory;
            if Init_Result /= Success then
               Cxos.Serial.Put_String ("Error marking kernel memory"
                 & ASCII.LF);
               return;
            end if;
            Cxos.Serial.Put_String ("Finished marking kernel memory"
              & ASCII.LF);
         end Mark_Kernel_Memory;

      --  Initialise peripheral devices.
      Initialise_Devices :
         declare
            use Cxos.PCI;

            --  The result of initialising the PCI functionality.
            Pci_Init_Result : Cxos.PCI.Process_Result;
         begin
            Pci_Init_Result := Cxos.PCI.Find_Pci_Devices;
            if Pci_Init_Result /= Success then
               return;
            end if;
         end Initialise_Devices;

      Initialise_Processes :
         begin
            Cxos.Process.Initialise;
         end Initialise_Processes;
   exception
      when Constraint_Error =>
         return;
   end Initialise_Kernel;

end Cxos.Boot;
