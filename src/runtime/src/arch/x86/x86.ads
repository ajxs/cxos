with Multiboot;
with System;

-------------------------------------------------------------------------------
--  SYSTEM.X86
--
--  Purpose:
--    This package contains initialisation code for the x86 system.
--    The initialisation procedure here will perform all the required
--    initialisation code for the platform.
-------------------------------------------------------------------------------
package x86 is
   pragma Preelaborate (x86);

   use Multiboot;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the x86 platform.
   --    This will perform all the necessary initialisation in order to load
   --    and begin execution of the kernel.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise (
     Magic_Number      : Multiboot_Magic_Number;
     Boot_Info_Address : System.Address
   )
   with Export,
     Convention => Assembler,
     External_Name => "__system_init";

private
   ----------------------------------------------------------------------------
   --  Install_Exception_Handlers
   --
   --  Purpose:
   --    This procedure installs the processor exception handlers.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Install_Exception_Handlers;

   ----------------------------------------------------------------------------
   --  Last_Chance_Handler
   --
   --  Purpose:
   --    The runtime Last_Chance_Handler function.
   --    This procedure is the GNAT mandated handler for any uncaught
   --    exceptions that are propagated to the top level.
   --    This runtime, like other bareboard targets, does not support exception
   --    propagation. So any uncaught exception will be handled here.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Last_Chance_Handler (
     Msg  : System.Address;
     Line : Integer
   ) with Export,
     Convention => C,
     External_Name => "__gnat_last_chance_handler";

   ----------------------------------------------------------------------------
   --  Protected_Mode_Init
   --
   --  Purpose:
   --    Performs the final jump to protected mode.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Protected_Mode_Init
   with Import,
     Convention    => C,
     External_Name => "__pmode_init";
end x86;
