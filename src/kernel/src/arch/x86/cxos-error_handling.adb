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

with Ada.Characters.Latin_1;
with Cxos.Debug;

package body Cxos.Error_Handling is
   package Chars renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------------
   --  Last_Chance_Handler
   ----------------------------------------------------------------------------
   procedure Last_Chance_Handler (
     Msg  : System.Address;
     Line : Integer
   ) is
   begin
      null;
   end Last_Chance_Handler;

   ----------------------------------------------------------------------------
   --  Log_Kernel_Error
   ----------------------------------------------------------------------------
   procedure Log_Kernel_Error (
     Message : String
   ) is
   begin
      if DEBUG_PRINT_ERRORS then
         Cxos.Debug.Put_String (Message & Chars.LF);
      end if;
   end Log_Kernel_Error;
end Cxos.Error_Handling;
