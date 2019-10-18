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
--  SYSTEM.X86.MEMORY.MAP
--
--  Purpose:
--    This package contains code and defintions for working with a map of
--    usable memory on the system.
-------------------------------------------------------------------------------
package x86.Memory.Map is
   pragma Preelaborate (x86.Memory.Map);

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the system memory map.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Init;

private
   ----------------------------------------------------------------------------
   --  Memory Map Frame type.
   --  Represents the presence of an individual page frame in memory.
   ----------------------------------------------------------------------------
   type Memory_Map_Frame is new Boolean
   with Size => 1;

   ----------------------------------------------------------------------------
   --  Memory Map Array type.
   --  Represents all page frames across the full linear address space.
   ----------------------------------------------------------------------------
   type Memory_Map_Array is array (Natural range 0 .. 16#100000#)
     of Memory_Map_Frame;

end x86.Memory.Map;
