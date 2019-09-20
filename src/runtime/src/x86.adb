with x86.GDT;
with x86.Serial;

package body x86 is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      x86.Serial.Initialise (x86.Serial.COM1, 38400);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising GDT" & ASCII.LF);
      x86.GDT.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1, "Flushing GDT" & ASCII.LF);
      x86.GDT.Finalise;

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
