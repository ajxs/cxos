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

with Ada.Characters.Latin_1;
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
      package Chars renames Ada.Characters.Latin_1;
   begin
      --  Initialise the COM1 Serial port, which will be used for all
      --  subsequent debugging output.
      x86.Serial.Initialise (x86.Serial.COM1, 38400);
      x86.Serial.Put_String (x86.Serial.COM1,
        "COM1 initialised" & Chars.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising PIC" & Chars.LF);
      x86.PIC.Initialise;

      --  Clear interrupts.
      x86.Interrupts.Set_Interrupt_Flag (False);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising GDT" & Chars.LF);
      x86.GDT.Initialise;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Finished initialising GDT" & Chars.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising IDT" & Chars.LF);
      x86.IDT.Initialise;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Finished initialising IDT" & Chars.LF);

      x86.Serial.Put_String (x86.Serial.COM1, "Loading IDT" & Chars.LF);
      x86.IDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1, "Flushing GDT" & Chars.LF);
      x86.GDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Jumping to protected mode" & Chars.LF);
      Protected_Mode_Init;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Protected mode entered" & Chars.LF);

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
