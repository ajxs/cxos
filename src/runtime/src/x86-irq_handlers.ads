-------------------------------------------------------------------------------
--  x86.IRQ_Handlers
--
--  Purpose:
--    This package contains code for handling individual IRQ requests.
-------------------------------------------------------------------------------
package x86.IRQ_Handlers is
   pragma Preelaborate (x86.IRQ_Handlers);

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
     Convention => C,
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
     Convention    => C,
     External_Name => "__irq1_entry";

private
   ----------------------------------------------------------------------------
   --  IRQ0_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ0 exception.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ0_Internal_Handler
   with Export,
     Convention    => C,
     External_Name => "__irq0_handler";

   ----------------------------------------------------------------------------
   --  IRQ1_Internal_Handler
   --
   --  Purpose:
   --    This function contains the Ada code for handling the IRQ1 exception.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure IRQ1_Internal_Handler
   with Export,
     Convention    => C,
     External_Name => "__irq1_handler";
end x86.IRQ_Handlers;
