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

with System.Storage_Elements; use System.Storage_Elements;

package body x86.PIC is
   ----------------------------------------------------------------------------
   --  Get_Port_Address
   ----------------------------------------------------------------------------
   function Get_Controller_Base_Address (
      Controller : PIC_Controller
   ) return System.Address is
   begin
      case Controller is
         when PIC1 =>
            return To_Address (16#20#);
         when PIC2 =>
            return To_Address (16#A0#);
      end case;
   exception
      when Constraint_Error =>
         return System.Null_Address;
   end Get_Controller_Base_Address;

end x86.PIC;
