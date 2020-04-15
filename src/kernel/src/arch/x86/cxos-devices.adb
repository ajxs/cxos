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

with Cxos.Devices.ATA;
with Cxos.Devices.PCI;

package body Cxos.Devices is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
      --  The result of internal functionality.
      Result : Process_Result;
   begin
      Result := Cxos.Devices.PCI.Find_Pci_Devices;
      if Result /= Success then
         return;
      end if;

      Cxos.Devices.ATA.Find_ATA_Devices;
   exception
      when Constraint_Error =>
         return;
   end Initialise;

end Cxos.Devices;
