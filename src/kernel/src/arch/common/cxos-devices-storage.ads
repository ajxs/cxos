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

with Interfaces; use Interfaces;

-------------------------------------------------------------------------------
--  CXOS.DEVICES.STORAGE
--
--  Purpose:
--    This package contains definitons and functionality for working with
--    storage devices.
-------------------------------------------------------------------------------
package Cxos.Devices.Storage is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Storage device read buffer type.
   --  Acts as a generic buffer type to use when reading from storage devices.
   ----------------------------------------------------------------------------
   type Read_Buffer is array (Natural range <>) of Unsigned_8;
end Cxos.Devices.Storage;
