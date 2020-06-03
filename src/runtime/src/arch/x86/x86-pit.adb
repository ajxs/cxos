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
with System.Storage_Elements; use System.Storage_Elements;

package body x86.PIT is
   ----------------------------------------------------------------------------
   --  Get_Register_Address
   --
   --  Implementation Notes:
   --   - Returns a constant value stored within the function.
   ----------------------------------------------------------------------------
   function Get_Register_Address (
     Register : PIT_Register
   ) return System.Address is
      Channel_0_Address : constant System.Address := To_Address (16#40#);
      Channel_1_Address : constant System.Address := To_Address (16#41#);
      Channel_2_Address : constant System.Address := To_Address (16#42#);
      Command_Address   : constant System.Address := To_Address (16#43#);
   begin
      case Register is
         when Channel_0_Data =>
            return Channel_0_Address;
         when Channel_1_Data =>
            return Channel_1_Address;
         when Channel_2_Data =>
            return Channel_2_Address;
         when Command =>
            return Command_Address;
      end case;
   exception
      when Constraint_Error =>
         return System.Null_Address;
   end Get_Register_Address;

end x86.PIT;
