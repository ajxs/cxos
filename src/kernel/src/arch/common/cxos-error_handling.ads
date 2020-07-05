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
--  CXOS.ERROR_HANDLING
--
--  Purpose:
--    This package contains functionality for reporting and handling kernel
--    errors.
-------------------------------------------------------------------------------
package Cxos.Error_Handling is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Log_Kernel_Error
   --
   --  This procedure logs a kernel error, logging and outputting the error
   --  string as configured.
   ----------------------------------------------------------------------------
   procedure Log_Kernel_Error (
     Message : String
   );

private
   ----------------------------------------------------------------------------
   --  Whether errors should be printed to the debug output.
   ----------------------------------------------------------------------------
   DEBUG_PRINT_ERRORS : constant Boolean := True;

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

end Cxos.Error_Handling;
