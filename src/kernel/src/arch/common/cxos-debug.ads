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
--  CXOS.DEBUG
--
--  Purpose:
--    This package contains logic for debugging kernel functionality.
--    This is a generic package. The debug methods are architecture-specific.
-------------------------------------------------------------------------------
package Cxos.Debug is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Put_String
   ----------------------------------------------------------------------------
   procedure Put_String (
     Data : String
   );

   ----------------------------------------------------------------------------
   --  Put_String_Wide
   ----------------------------------------------------------------------------
   procedure Put_String_Wide (
     Data : Wide_String
   );

end Cxos.Debug;
