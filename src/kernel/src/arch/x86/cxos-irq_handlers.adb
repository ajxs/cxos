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

with Cxos.Debug;
with Cxos.Time_Keeping;
with Interfaces;
with System.Storage_Elements;
with x86.Interrupts.Names;
with x86.PIC;
with x86.Port_IO;

package body Cxos.IRQ_Handlers is
   use x86.Interrupts.Names;
   use Interfaces;
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  IRQ0_Internal_Handler
   --
   --  Implementation Notes:
   --   - Calls the system tick handler to signal that a timer tick has
   --     occurred. This will increment the system's internal time.
   ----------------------------------------------------------------------------
   procedure IRQ0_Internal_Handler is
   begin
      --  Trigger the internal System Tick Handler to signal that a
      --  timer tick has occurred.
      Cxos.Time_Keeping.System_Tick_Handler;
      x86.PIC.Send_EOI (IRQ0);

   end IRQ0_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ10_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ10_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ10);
   end IRQ10_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ11_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ11_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ11);
   end IRQ11_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ12_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ12_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ12);
   end IRQ12_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ13_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ13_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ13);
   end IRQ13_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ14_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ14_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ14);
   end IRQ14_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ15_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ15_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ15);
   end IRQ15_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ1_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ1_Internal_Handler is
      --  Stores the keycode triggering the interrupt.
      Keycode : Unsigned_8;
   begin
      --  Read the keycode.
      Keycode := x86.Port_IO.Inb (To_Address (16#60#));

      Cxos.Debug.Put_String ("" & Character'Val (Keycode));

      x86.PIC.Send_EOI (IRQ1);

   end IRQ1_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ2_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ2_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ2);
   end IRQ2_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ3_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ3_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ3);
   end IRQ3_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ4_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ4_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ4);
   end IRQ4_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ5_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ5_Internal_Handler is
   begin

      x86.PIC.Send_EOI (IRQ5);

   end IRQ5_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ6_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ6_Internal_Handler is
   begin

      x86.PIC.Send_EOI (IRQ6);

   end IRQ6_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ7_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ7_Internal_Handler is
   begin

      x86.PIC.Send_EOI (IRQ7);

   end IRQ7_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ8_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ8_Internal_Handler is
   begin

      x86.PIC.Send_EOI (IRQ8);

   end IRQ8_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ9_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ9_Internal_Handler is
   begin

      x86.PIC.Send_EOI (IRQ9);

   end IRQ9_Internal_Handler;

end Cxos.IRQ_Handlers;
