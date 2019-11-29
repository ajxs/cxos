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
with System.Storage_Elements;
with x86.PIT;

package body Cxos.PIT is
   use Interfaces;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Implementation Notes:
   --   - Only initialises Channel 0.
   ----------------------------------------------------------------------------
   procedure Initialise is
      use x86.PIT;

      --  The predefined initial value to select for a 100hz rate.
      INITIAL_VALUE_100HZ : constant Unsigned_16 := 11931;
   begin
      --  Initialise Channel 0 to pulse at 100hz, using operating mode 2,
      --  raising an interrupt every 10ms.
      x86.PIT.Initialise_Channel (Channel_0, Rate_Generator,
        INITIAL_VALUE_100HZ);
   end Initialise;

end Cxos.PIT;
