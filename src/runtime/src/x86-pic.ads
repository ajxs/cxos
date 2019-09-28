-------------------------------------------------------------------------------
--  X86.PIC
--
--  Purpose:
--    This package contains code for working with the x86 programmable
--    interrupt controller.
-------------------------------------------------------------------------------
package x86.PIC is
   pragma Preelaborate (x86.PIC);

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

end x86.PIC;
