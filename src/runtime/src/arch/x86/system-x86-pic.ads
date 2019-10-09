with Ada.Interrupts;

-------------------------------------------------------------------------------
--  SYSTEM.X86.PIC
--
--  Purpose:
--    This package contains code for working with the x86 8259A programmable
--    interrupt controller.
-------------------------------------------------------------------------------
package System.x86.PIC is
   pragma Preelaborate (System.x86.PIC);

   use Ada.Interrupts;

   ----------------------------------------------------------------------------
   --  PIC Controller type.
   --  Used in selecting which PIC to perform an operation on.
   ----------------------------------------------------------------------------
   type PIC_Controller is (
     PIC1,
     PIC2
   );

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

private
   ----------------------------------------------------------------------------
   --  Install_Descriptor
   --
   --  Purpose:
   --    This function gets the base address for a particular PIC controller.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Get_Controller_Base_Address (
      Controller : PIC_Controller
   ) return System.Address;

end System.x86.PIC;
