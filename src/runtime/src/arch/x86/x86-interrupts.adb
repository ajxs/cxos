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

with System.Machine_Code;

package body x86.Interrupts is
   ----------------------------------------------------------------------------
   --  Set_Interrupt_Flag
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Flag (
     Status : Boolean
   ) is
   begin
      case Status is
         when True =>
            System.Machine_Code.Asm (
              Template => "sti",
              Volatile => True);
         when False =>
            System.Machine_Code.Asm (
              Template => "cli",
              Volatile => True);
      end case;
   exception
      when Constraint_Error =>
         return;
   end Set_Interrupt_Flag;
end x86.Interrupts;
