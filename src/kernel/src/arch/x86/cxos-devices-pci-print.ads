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

-------------------------------------------------------------------------------
--  CXOS.DEVICES.PCI.PRINT
--
--  Purpose:
--    This package contains code for printing information about PCI devices.
-------------------------------------------------------------------------------
private package Cxos.Devices.PCI.Print is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Print_Pci_Device
   --
   --  Purpose:
   --    This procedure prints information about a PCI device record.
   ----------------------------------------------------------------------------
   procedure Print_PCI_Device (
     Device : PCI_Device_T
   );

end Cxos.Devices.PCI.Print;
