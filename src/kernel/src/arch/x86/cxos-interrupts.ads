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
--  CXOS.INTERRUPTS
--
--  Purpose:
--    This package contains code and definitions for setting and working with
--    interrupts.
-------------------------------------------------------------------------------
package Cxos.Interrupts is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Process Result type.
   --  Used for tracking the results of package processes and reporting
   --  specific errors to the caller.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Failure,
     Success
   );

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises interrupt handlers for the kernel.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Initialise return Process_Result;

   ----------------------------------------------------------------------------
   --  Set_Interrupt_Flag
   --
   --  Purpose:
   --    Sets or clears the interrupt flag in the processor EFLAGS register.
   --    If this is set to true, the processor begins responding to external
   --    maskable interrupts. Otherwise all maskable interrupts are ignored.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Flag (
     Status : Boolean
   );
end Cxos.Interrupts;
