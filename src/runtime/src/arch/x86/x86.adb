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

with Ada.Interrupts.Names;
with Interfaces;
with x86.Exceptions;
with x86.IDT;
with x86.Interrupts;
with x86.IRQ_Handlers;
with x86.GDT;
with x86.PIC;
with x86.Memory.Map;
with x86.PIT;
with x86.Serial;
with x86.Time_Keeping;
with x86.Vga;

package body x86 is
   use Ada.Interrupts.Names;
   use Interfaces;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise (
     Magic_Number      : Multiboot_Magic_Number;
     Boot_Info_Address : System.Address
   ) is
      --  Create multiboot info structure overlaid at boot info address.
      Boot_Info : constant Multiboot_Info
      with Address => Boot_Info_Address,
        Import,
        Convention => C,
        Volatile;
   begin
      --  Initialise VGA system.
      x86.Vga.Clear (x86.Vga.Black);
      x86.Vga.Put_String (0, 0, x86.Vga.Light_Green, x86.Vga.Black,
        "VGA Text Mode Initialised");

      --  Initialise the COM1 Serial port, which will be used for all
      --  subsequent debugging output.
      x86.Serial.Initialise (x86.Serial.COM1, 38400);
      x86.Serial.Put_String (x86.Serial.COM1,
        "COM1 initialised" & ASCII.LF);

      --  Check whether we were booted by a Multiboot compatible bootloader.
      if Magic_Number = VALID_MAGIC_NUMBER then
         x86.Serial.Put_String (x86.Serial.COM1,
           "Detected valid Multiboot magic number" & ASCII.LF);
      else
         x86.Serial.Put_String (x86.Serial.COM1,
           "Unable to detect valid Multiboot magic number" & ASCII.LF);
      end if;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising PIC" & ASCII.LF);
      x86.PIC.Initialise;

      --  Clear interrupts.
      x86.Interrupts.Set_Interrupt_Flag (False);

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising GDT" & ASCII.LF);
      x86.GDT.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising IDT" & ASCII.LF);
      x86.IDT.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Installing processor exception handlers" & ASCII.LF);
      Install_Exception_Handlers;

      --  Install a handler for IRQ0.
      x86.PIC.Set_Interrupt_Mask (IRQ0, False);
      x86.IDT.Install_Descriptor (32,
        x86.IRQ_Handlers.IRQ0_Handler'Address, 16#8#);

      --  Install a handler for IRQ1.
      x86.PIC.Set_Interrupt_Mask (IRQ1, False);
      x86.IDT.Install_Descriptor (33,
        x86.IRQ_Handlers.IRQ1_Handler'Address, 16#8#);

      x86.IDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Flushing GDT" & ASCII.LF);
      x86.GDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Jumping to protected mode" & ASCII.LF);
      Protected_Mode_Init;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Protected mode entered" & ASCII.LF);

      --  Initialise system timer and PIT before re-enabling interrupt
      --  generation.
      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising system timer" & ASCII.LF);
      x86.Time_Keeping.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising PIT" & ASCII.LF);
      x86.PIT.Initialise;

      --  Enable interrupts.
      x86.Interrupts.Set_Interrupt_Flag (True);

      --  Initialise the system memory map.
      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising Memory Map" & ASCII.LF);
      x86.Memory.Map.Initialise;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Install_Exception_Handlers
   ----------------------------------------------------------------------------
   procedure Install_Exception_Handlers is
   begin
      x86.IDT.Install_Descriptor (0,
        x86.Exceptions.Exception_0_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (1,
        x86.Exceptions.Exception_1_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (2,
        x86.Exceptions.Exception_2_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (3,
        x86.Exceptions.Exception_3_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (4,
        x86.Exceptions.Exception_4_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (5,
        x86.Exceptions.Exception_5_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (6,
        x86.Exceptions.Exception_6_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (7,
        x86.Exceptions.Exception_7_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (8,
        x86.Exceptions.Exception_8_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (9,
        x86.Exceptions.Exception_9_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (10,
        x86.Exceptions.Exception_10_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (11,
        x86.Exceptions.Exception_11_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (12,
        x86.Exceptions.Exception_12_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (13,
        x86.Exceptions.Exception_13_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (14,
        x86.Exceptions.Exception_14_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (15,
        x86.Exceptions.Exception_15_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (16,
        x86.Exceptions.Exception_16_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (17,
        x86.Exceptions.Exception_17_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (18,
        x86.Exceptions.Exception_18_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (19,
        x86.Exceptions.Exception_19_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (20,
        x86.Exceptions.Exception_20_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (21,
        x86.Exceptions.Exception_21_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (22,
        x86.Exceptions.Exception_22_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (23,
        x86.Exceptions.Exception_23_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (24,
        x86.Exceptions.Exception_24_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (25,
        x86.Exceptions.Exception_25_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (26,
        x86.Exceptions.Exception_26_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (27,
        x86.Exceptions.Exception_27_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (28,
        x86.Exceptions.Exception_28_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (29,
        x86.Exceptions.Exception_29_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (30,
        x86.Exceptions.Exception_30_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (31,
        x86.Exceptions.Exception_31_Handler'Address, 16#8#);

   end Install_Exception_Handlers;

   ----------------------------------------------------------------------------
   --  Last_Chance_Handler
   ----------------------------------------------------------------------------
   procedure Last_Chance_Handler (
     Msg  : System.Address;
     Line : Integer
   ) is
   begin
      null;
   end Last_Chance_Handler;

end x86;
