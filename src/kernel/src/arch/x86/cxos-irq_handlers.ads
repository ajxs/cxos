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
--  CXOS.IRQ_HANDLERS
--
--  Purpose:
--    This package contains code for handling individual IRQ requests.
--    Interrupt request handlers are implemented by wrapping each individual
--    interrupt handler in an 'entry' function, implemented in assembler. This
--    'entry function' is responsible for preserving the machine state and
--    restoring the proper machine state after the interrupt handler has
--    successfully exited. Refer to the implementation of each 'entry function'
--    in 'cxos-irq_handlers-entry.S'.
-------------------------------------------------------------------------------
package Cxos.IRQ_Handlers is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  IRQ0_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ0.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ0_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq0_entry";

   ----------------------------------------------------------------------------
   --  IRQ1_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ1.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ1_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq1_entry";

   ----------------------------------------------------------------------------
   --  IRQ2_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ2.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ2_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq2_entry";

   ----------------------------------------------------------------------------
   --  IRQ3_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ3.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ3_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq3_entry";

   ----------------------------------------------------------------------------
   --  IRQ4_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ4.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ4_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq4_entry";

   ----------------------------------------------------------------------------
   --  IRQ5_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ5.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ5_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq5_entry";

   ----------------------------------------------------------------------------
   --  IRQ6_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ6.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ6_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq6_entry";

   ----------------------------------------------------------------------------
   --  IRQ7_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ7.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ7_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq7_entry";

   ----------------------------------------------------------------------------
   --  IRQ8_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ8.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ8_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq8_entry";

   ----------------------------------------------------------------------------
   --  IRQ9_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ9.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ9_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq9_entry";

   ----------------------------------------------------------------------------
   --  IRQ10_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ10.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ10_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq10_entry";

   ----------------------------------------------------------------------------
   --  IRQ11_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ11.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ11_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq11_entry";

   ----------------------------------------------------------------------------
   --  IRQ12_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ12.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ12_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq12_entry";

   ----------------------------------------------------------------------------
   --  IRQ13_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ13.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ13_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq13_entry";

   ----------------------------------------------------------------------------
   --  IRQ14_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ14.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ14_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq14_entry";

   ----------------------------------------------------------------------------
   --  IRQ15_Handler
   --
   --  Purpose:
   --    This function handles the request for IRQ15.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ15_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__irq15_entry";

private
   ----------------------------------------------------------------------------
   --  IRQ0_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ0 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ0_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq0_handler";

   ----------------------------------------------------------------------------
   --  IRQ1_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ1 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ1_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq1_handler";

   ----------------------------------------------------------------------------
   --  IRQ2_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ2 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ2_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq2_handler";

   ----------------------------------------------------------------------------
   --  IRQ3_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ3 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ3_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq3_handler";

   ----------------------------------------------------------------------------
   --  IRQ4_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ4 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ4_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq4_handler";

   ----------------------------------------------------------------------------
   --  IRQ5_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ5 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ5_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq5_handler";

   ----------------------------------------------------------------------------
   --  IRQ6_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ6 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ6_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq6_handler";

   ----------------------------------------------------------------------------
   --  IRQ7_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ7 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ7_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq7_handler";

   ----------------------------------------------------------------------------
   --  IRQ8_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ8 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ8_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq8_handler";

   ----------------------------------------------------------------------------
   --  IRQ9_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ9 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ9_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq9_handler";

   ----------------------------------------------------------------------------
   --  IRQ10_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ10 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ10_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq10_handler";

   ----------------------------------------------------------------------------
   --  IRQ11_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ11 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ11_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq11_handler";

   ----------------------------------------------------------------------------
   --  IRQ12_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ12 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ12_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq12_handler";

   ----------------------------------------------------------------------------
   --  IRQ13_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ13 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ13_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq13_handler";

   ----------------------------------------------------------------------------
   --  IRQ14_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ14 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ14_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq14_handler";

   ----------------------------------------------------------------------------
   --  IRQ15_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ15 interrupt.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ15_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__irq15_handler";

end Cxos.IRQ_Handlers;
