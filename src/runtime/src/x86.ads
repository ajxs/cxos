with System;

-------------------------------------------------------------------------------
--  X86
--
--  Purpose:
--    This package contains initialisation code for the x86 system.
--    The initialisation procedure here will perform all the required
--    initialisation code for the platform.
-------------------------------------------------------------------------------
package x86 is
   pragma Preelaborate (x86);

   procedure Initialise
   with Export,
     Convention => C,
     External_Name => "__system_init";

   procedure Last_Chance_Handler (
     Msg  : System.Address;
     Line : Integer
   ) with Export,
     Convention => C,
     External_Name => "__gnat_last_chance_handler";
end x86;
