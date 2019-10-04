with Ada.Interrupts.Names;
with Interfaces;
with System.Storage_Elements;
with x86.PIC;
with x86.Port_IO;
with x86.Serial;

package body x86.IRQ_Handlers is
   use Ada.Interrupts.Names;
   use Interfaces;
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  IRQ0_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ0_Internal_Handler is
   begin
      x86.PIC.Send_EOI (IRQ0);
   end IRQ0_Internal_Handler;

   ----------------------------------------------------------------------------
   --  IRQ1_Internal_Handler
   ----------------------------------------------------------------------------
   procedure IRQ1_Internal_Handler is
      --  Stores the keycode triggering the interrupt.
      Keycode : Unsigned_8;
   begin
      --  Read the keycode.
      Keycode := x86.Port_IO.Inb (To_Address (16#60#));

      x86.Serial.Put_String (x86.Serial.COM1, "" & Character'Val (Keycode));

      x86.PIC.Send_EOI (IRQ1);

   end IRQ1_Internal_Handler;
end x86.IRQ_Handlers;
