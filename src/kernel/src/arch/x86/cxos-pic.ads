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

with Ada.Interrupts; use Ada.Interrupts;

-------------------------------------------------------------------------------
--  CXOS.DEVICES.PIC
--
--  Purpose:
--    This package contains code for working with the x86 8259A programmable
--    interrupt controller.
-------------------------------------------------------------------------------
package Cxos.PIC is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the x86 PIC.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise;

   ----------------------------------------------------------------------------
   --  Send_EOI
   --
   --  Purpose:
   --    This function sends an EOI signal to the PIC controller.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Send_EOI (
     IRQ : Interrupt_ID
   );

   ----------------------------------------------------------------------------
   --  Set_Interrupt_Mask
   --
   --  Purpose:
   --    Sets the mask status of an individual interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Mask (
     IRQ    : Interrupt_ID;
     Status : Boolean
   );

end Cxos.PIC;
