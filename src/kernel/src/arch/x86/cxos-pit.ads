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
with x86.PIT; use x86.PIT;

-------------------------------------------------------------------------------
--  CXOS.PIT
--
--  Purpose:
--    This package contains code for working with the x86 8253/8254
--    Programmable Interval Timer.
-------------------------------------------------------------------------------
package Cxos.PIT is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the x86 PIT.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise;

private
   ----------------------------------------------------------------------------
   --  Initialise_Channel
   --
   --  Purpose:
   --    This procedure initialises an individual PIT channel. This sets the
   --    initial value for the channel as well as its operating mode.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise_Channel (
     Channel        : PIT_Channel_T;
     Operating_Mode : Operating_Mode_Type;
     Initial_Value  : Unsigned_16
   );

end Cxos.PIT;
