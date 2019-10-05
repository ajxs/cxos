with Ada.Interrupts.Names;
with Interfaces;
with System.Storage_Elements;
with System.x86.PIC;
with System.x86.Port_IO;
with System.x86.Serial;

package body System.x86.IRQ_Handlers is
   use Ada.Interrupts.Names;
   use Interfaces;
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  IRQ0_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ0_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ0);
   end IRQ0_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ10_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ10_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ10);
   end IRQ10_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ11_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ11_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ11);
   end IRQ11_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ12_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ12_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ12);
   end IRQ12_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ13_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ13_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ13);
   end IRQ13_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ14_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ14_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ14);
   end IRQ14_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ15_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ15_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ15);
   end IRQ15_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ1_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ1_Internal_Handler is
      --  Stores the keycode triggering the interrupt.
      Keycode : Unsigned_8;
   begin
      --  Read the keycode.
      Keycode := System.x86.Port_IO.Inb (To_Address (16#60#));

      System.x86.Serial.Put_String (System.x86.Serial.COM1, "" & Character'Val (Keycode));

      System.x86.PIC.Send_EOI (IRQ1);

   end IRQ1_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ2_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ2_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ2);
   end IRQ2_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ3_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ3_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ3);
   end IRQ3_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ4_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ4_Internal_Handler is
   begin
      System.x86.PIC.Send_EOI (IRQ4);
   end IRQ4_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ5_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ5_Internal_Handler is
   begin

      System.x86.PIC.Send_EOI (IRQ5);

   end IRQ5_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ6_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ6_Internal_Handler is
   begin

      System.x86.PIC.Send_EOI (IRQ6);

   end IRQ6_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ7_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ7_Internal_Handler is
   begin

      System.x86.PIC.Send_EOI (IRQ7);

   end IRQ7_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ8_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ8_Internal_Handler is
   begin

      System.x86.PIC.Send_EOI (IRQ8);

   end IRQ8_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ9_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ9_Internal_Handler is
   begin

      System.x86.PIC.Send_EOI (IRQ9);

   end IRQ9_Internal_Handler;

end System.x86.IRQ_Handlers;
