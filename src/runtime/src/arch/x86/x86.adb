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

with x86.IDT;
with x86.Interrupts;
with x86.GDT;
with x86.PIC;
with x86.Serial;

package body x86 is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      --  Initialise the COM1 Serial port, which will be used for all
      --  subsequent debugging output.
      x86.Serial.Initialise (x86.Serial.COM1, 38400);
      x86.Serial.Put_String (x86.Serial.COM1,
        "COM1 initialised" & ASCII.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising PIC" & ASCII.LF);
      x86.PIC.Initialise;

      --  Clear interrupts.
      x86.Interrupts.Set_Interrupt_Flag (False);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising GDT" & ASCII.LF);
      x86.GDT.Initialise;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Finished initialising GDT" & ASCII.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising IDT" & ASCII.LF);
      x86.IDT.Initialise;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Finished initialising IDT" & ASCII.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Loading IDT" & ASCII.LF);
      x86.IDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1, "Flushing GDT" & ASCII.LF);
      x86.GDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Jumping to protected mode" & ASCII.LF);
      Protected_Mode_Init;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Protected mode entered" & ASCII.LF);

      --  Enable interrupts.
      x86.Interrupts.Set_Interrupt_Flag (True);
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
