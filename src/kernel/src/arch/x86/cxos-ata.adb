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

with Cxos.Serial;
with x86.ATA;

package body Cxos.ATA is
   procedure Initialise is
      use x86.ATA;

      Device_Type : x86.ATA.ATA_Device_Type;
   begin
      x86.ATA.Reset_Bus (Primary);

      Device_Type := x86.ATA.Get_Device_Type (Primary, Master);
      case Device_Type is
         when PATAPI =>
            Cxos.Serial.Put_String ("PATAPI" & ASCII.LF);
         when SATAPI =>
            Cxos.Serial.Put_String ("PATAPI" & ASCII.LF);
         when PATA   =>
            Cxos.Serial.Put_String ("PATA" & ASCII.LF);
         when SATA   =>
            Cxos.Serial.Put_String ("SATA" & ASCII.LF);
         when Unknown_ATA_Device =>
            Cxos.Serial.Put_String ("Unknown" & ASCII.LF);
      end case;
   exception
      when Constraint_Error =>
         return;
   end Initialise;
end Cxos.ATA;
