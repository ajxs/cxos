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
--  SYSTEM.X86.INTERRUPTS
--
--  Purpose:
--    This package contains code for managing x86 processor interrupts.
-------------------------------------------------------------------------------
package x86.Interrupts is
   pragma Preelaborate (x86.Interrupts);

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
end x86.Interrupts;
