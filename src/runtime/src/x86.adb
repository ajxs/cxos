with Ada.Interrupts.Names;
with x86.IDT;
with x86.Interrupts;
with x86.IRQ_Handlers;
with x86.GDT;
with x86.PIC;
with x86.Serial;

package body x86 is
   use Ada.Interrupts.Names;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      x86.Serial.Initialise (x86.Serial.COM1, 38400);
      x86.Serial.Put_String (x86.Serial.COM1, "COM1 initialised" & ASCII.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising PIC" & ASCII.LF);
      x86.PIC.Initialise;

      --  Clear interrupts.
      x86.Interrupts.Set_Interrupt_Flag (False);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising GDT" & ASCII.LF);
      x86.GDT.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising IDT" & ASCII.LF);
      x86.IDT.Initialise;

      --  Install a handler for IRQ0.
      x86.PIC.Set_Interrupt_Mask (IRQ0, False);
      x86.IDT.Install_Descriptor (32,
        x86.IRQ_Handlers.IRQ0_Handler'Address, 16#8#);

      --  Install a handler for IRQ1.
      x86.PIC.Set_Interrupt_Mask (IRQ1, False);
      x86.IDT.Install_Descriptor (33,
        x86.IRQ_Handlers.IRQ1_Handler'Address, 16#8#);

      x86.IDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1, "Flushing GDT" & ASCII.LF);
      x86.GDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Jumping to protected mode" & ASCII.LF);
      Protected_Mode_Init;

      --  Enable interrupts.
      x86.Interrupts.Set_Interrupt_Flag (True);

      x86.Serial.Put_String (x86.Serial.COM1,
        "Protected mode entered" & ASCII.LF);

   end Initialise;

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
