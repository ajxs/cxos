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
   pragma Preelaborate;

   type Kernel_Process_Result is (
     Failure,
     Success
   );

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the x86 platform.
   --    This will perform all the necessary system initialisation necessary
   --    prior to beginning kernel execution.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise
   with Export,
     Convention => Assembler,
     External_Name => "__system_init";

private
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
     Convention    => Assembler,
     External_Name => "__protected_mode_init";
end x86;
