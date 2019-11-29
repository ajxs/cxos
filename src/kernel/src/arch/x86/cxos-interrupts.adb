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

with Cxos.IRQ_Handlers;
with x86.IDT;
with x86.Interrupts.Names;
with x86.PIC;

package body Cxos.Interrupts is
   use x86.Interrupts.Names;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   function Initialise return Process_Result is
   begin
      --  Install a handler for IRQ0.
      x86.PIC.Set_Interrupt_Mask (IRQ0, False);
      x86.IDT.Install_Descriptor (32,
        Cxos.IRQ_Handlers.IRQ0_Handler'Address, 16#8#);

      --  Install a handler for IRQ1.
      x86.PIC.Set_Interrupt_Mask (IRQ1, False);
      x86.IDT.Install_Descriptor (33,
        Cxos.IRQ_Handlers.IRQ1_Handler'Address, 16#8#);

      return Success;
   end Initialise;
end Cxos.Interrupts;
