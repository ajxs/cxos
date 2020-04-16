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
--  CXOS.DEVICES.SERIAL
--
--  Purpose:
--    This package contains functionality for interfacing with the target
--    architecture's serial hardware.
-------------------------------------------------------------------------------
package Cxos.Devices.Serial is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Put_String
   --
   --  Purpose:
   --    This procedure prints a string to the default output serial port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Put_String (
     Data : String
   );
end Cxos.Devices.Serial;
