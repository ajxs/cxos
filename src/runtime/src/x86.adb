with x86.IDT;
with x86.GDT;
with x86.PIC;
with x86.Serial;

package body x86 is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      x86.Serial.Initialise (x86.Serial.COM1, 38400);
      x86.Serial.Put_String (x86.Serial.COM1, "COM1 initialised" & ASCII.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising PIC" & ASCII.LF);
      x86.PIC.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising GDT" & ASCII.LF);
      x86.GDT.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising IDT" & ASCII.LF);
      x86.IDT.Initialise;
      x86.IDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1, "Flushing GDT" & ASCII.LF);
      x86.GDT.Finalise;

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
