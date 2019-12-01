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

with System.Storage_Elements;

package body x86.ATA is
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Get_Register_Address
   ----------------------------------------------------------------------------
   function Get_Register_Address (
     Bus      : ATA_Bus;
     Register : ATA_Regiser_Type
   ) return System.Address is
      --  Whether this register is located on the Base IO port.
      Bus_IO_Port     : Boolean         := False;
      --  The address base of the bus port.
      Bus_Base_Port   : Integer_Address := 0;
      --  The offset of this register from the bus base.
      Register_Offset : Integer_Address := 0;
   begin
      case Register is
         when Data           =>
            Bus_IO_Port     := True;
            Register_Offset := 0;
         when Error          =>
            Bus_IO_Port     := True;
            Register_Offset := 1;
         when Features       =>
            Bus_IO_Port     := True;
            Register_Offset := 1;
         when Sector_Count   =>
            Bus_IO_Port     := True;
            Register_Offset := 2;
         when Sector_Number  =>
            Bus_IO_Port     := True;
            Register_Offset := 3;
         when Cylinder_Low   =>
            Bus_IO_Port     := True;
            Register_Offset := 4;
         when Cylinder_High  =>
            Bus_IO_Port     := True;
            Register_Offset := 5;
         when Drive_Head     =>
            Bus_IO_Port     := True;
            Register_Offset := 6;
         when Device_Status  =>
            Bus_IO_Port     := True;
            Register_Offset := 7;
         when Command        =>
            Bus_IO_Port     := True;
            Register_Offset := 7;
         when Alt_Status     =>
            Bus_IO_Port     := False;
            Register_Offset := 0;
         when Device_Control =>
            Bus_IO_Port     := False;
            Register_Offset := 0;
         when Drive_Address  =>
            Bus_IO_Port     := False;
            Register_Offset := 1;
      end case;

      case Bus is
         when Primary =>
            if Bus_IO_Port then
               Bus_Base_Port := 16#1F0#;
            else
               Bus_Base_Port := 16#3F6#;
            end if;
         when Secondary =>
            if Bus_IO_Port then
               Bus_Base_Port := 16#1E8#;
            else
               Bus_Base_Port := 16#366#;
            end if;
      end case;

      return To_Address (Bus_Base_Port + Register_Offset);
   exception
      when Constraint_Error =>
         return System.Null_Address;
   end Get_Register_Address;

end x86.ATA;
