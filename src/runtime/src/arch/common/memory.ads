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

with Interfaces;
with System;

-------------------------------------------------------------------------------
--  Purpose:
--    This package contains code and defintions for working with memory common
--    to all platforms.
-------------------------------------------------------------------------------
package Memory is
   pragma Preelaborate (Memory);

   ----------------------------------------------------------------------------
   --  Byte Array Type.
   --  Used in memory operations. The type is aliased to ensure that the
   --  element at every index of the array is treated as a pointer and not
   --  stored in registers.
   ----------------------------------------------------------------------------
   type Byte_Array is array (Natural range <>)
     of aliased Interfaces.Unsigned_8;

   ----------------------------------------------------------------------------
   --  Copy
   --
   --  Purpose:
   --    Generic memcpy implementation.
   --    This is reuired by the runtime for default initialisation of
   --    package variables.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Copy (
     Dest   : System.Address;
     Source : System.Address;
     Count  : Integer
   ) return System.Address
   with Export,
     Convention    => C,
     External_Name => "memcpy";

end Memory;
