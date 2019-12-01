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

with Cxos.Interrupts;
with Cxos.Memory;
with Cxos.ATA;
with Cxos.PCI;
with Cxos.PIT;
with Cxos.Serial;
with Cxos.Time_Keeping;
with Cxos.VFS;
with System.Machine_Code;

package body Cxos is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   function Initialise_Kernel return Kernel_Init_Process_Result is
   begin
      --  Initialise system interrupts.
      Initialise_Interrupts :
         declare
            use Cxos.Interrupts;

            --  The result of the internal initialisation process.
            Init_Result : Cxos.Interrupts.Process_Result;
         begin
            Cxos.Serial.Put_String ("Initialising Interrupts" & ASCII.LF);

            Init_Result := Cxos.Interrupts.Initialise;
            if Init_Result /= Success then
               return Failure;
            end if;
         end Initialise_Interrupts;

      --  Initialise system timer and PIT before re-enabling interrupt
      --  generation.
      Initialise_Timers :
         begin
            Cxos.Serial.Put_String ("Initialising system timer" & ASCII.LF);
            Cxos.Time_Keeping.Initialise;

            Cxos.Serial.Put_String ("Initialising PIT" & ASCII.LF);
            Cxos.PIT.Initialise;
         end Initialise_Timers;

      --  Initialise the file system.
      Initialise_Filesystem :
         declare
            use Cxos.VFS;

            --  The result of initialising the virtual file system.
            Vfs_Init_Result : Cxos.VFS.Process_Result;
         begin
            Vfs_Init_Result := Cxos.VFS.Initialise;
            if Vfs_Init_Result /= Success then
               return Failure;
            end if;
         end Initialise_Filesystem;

      --  Initialise System Memory.
      Initialise_Memory :
         declare
            use Cxos.Memory;

            --  The result of the internal initialisation process.
            Init_Result : Cxos.Memory.Process_Result;
         begin
            Init_Result := Cxos.Memory.Initialise;
            if Init_Result /= Success then
               return Failure;
            end if;
         end Initialise_Memory;

      Initialise_Devices :
         declare
            use Cxos.PCI;

            --  The result of initialising the PCI functionality.
            Pci_Init_Result : Cxos.PCI.Process_Result;
         begin
            Pci_Init_Result := Cxos.PCI.Find_Pci_Devices;
            if Pci_Init_Result /= Success then
               return Failure;
            end if;
         end Initialise_Devices;

      Cxos.ATA.Initialise;

      return Success;
   exception
      when Constraint_Error =>
         return Failure;
   end Initialise_Kernel;

   ----------------------------------------------------------------------------
   --  Main
   ----------------------------------------------------------------------------
   procedure Main is
   begin
      --  Print the ASCII splash screen.
      Print_Splash;

      --  Loop forever.
      loop
         System.Machine_Code.Asm ("hlt", Volatile => True);
      end loop;
   end Main;

   ----------------------------------------------------------------------------
   --  Print_Splash
   ----------------------------------------------------------------------------
   procedure Print_Splash is
      Line_1 : constant String := "  /$$$$$$  /$$   /$$  /$$$$$$   /$$$$$$ ";
      Line_2 : constant String := " /$$__  $$| $$  / $$ /$$__  $$ /$$__  $$";
      Line_3 : constant String := "| $$  \__/|  $$/ $$/| $$  \ $$| $$  \__/";
      Line_4 : constant String := "| $$       \  $$$$/ | $$  | $$|  $$$$$$ ";
      Line_5 : constant String := "| $$        >$$  $$ | $$  | $$ \____  $$";
      Line_6 : constant String := "| $$    $$ /$$/\  $$| $$  | $$ /$$  \ $$";
      Line_7 : constant String := "|  $$$$$$/| $$  \ $$|  $$$$$$/|  $$$$$$/";
      Line_8 : constant String := " \______/ |__/  |__/ \______/  \______/ ";
   begin
      Print_Splash_to_Serial :
         begin
            Cxos.Serial.Put_String ("" & ASCII.LF);
            Cxos.Serial.Put_String (Line_1 & ASCII.LF);
            Cxos.Serial.Put_String (Line_2 & ASCII.LF);
            Cxos.Serial.Put_String (Line_3 & ASCII.LF);
            Cxos.Serial.Put_String (Line_4 & ASCII.LF);
            Cxos.Serial.Put_String (Line_5 & ASCII.LF);
            Cxos.Serial.Put_String (Line_6 & ASCII.LF);
            Cxos.Serial.Put_String (Line_7 & ASCII.LF);
            Cxos.Serial.Put_String (Line_8 & ASCII.LF);
            Cxos.Serial.Put_String ("" & ASCII.LF);
         end Print_Splash_to_Serial;
   end Print_Splash;
end Cxos;
