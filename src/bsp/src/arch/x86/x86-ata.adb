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
with x86.Port_IO;

package body x86.ATA is
   ----------------------------------------------------------------------------
   --  Get_Register_Address
   ----------------------------------------------------------------------------
   function Get_Register_Address (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type
   ) return System.Address is
      --  Whether this register is located on the Base IO port.
      Bus_IO_Port     : Boolean         := False;
      --  The address base of the bus port.
      Bus_Base_Port   : Integer_Address := 0;
      --  The offset of this register from the bus base.
      Register_Offset : Integer_Address := 0;
   begin
      case Register is
         when Data_Reg       =>
            Bus_IO_Port     := True;
            Register_Offset := 0;
         when Error_Reg      =>
            Bus_IO_Port     := True;
            Register_Offset := 1;
         when Features       =>
            Bus_IO_Port     := True;
            Register_Offset := 1;
         when Sector_Count_Reg =>
            Bus_IO_Port     := True;
            Register_Offset := 2;
         when Sector_Number_Reg =>
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
         when Command_Reg    =>
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

   ----------------------------------------------------------------------------
   --  Read_Byte_From_Register
   ----------------------------------------------------------------------------
   function Read_Byte_From_Register (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type
   ) return Unsigned_8 is
      Register_Port : System.Address;
   begin
      Register_Port := Get_Register_Address (Bus, Register);

      return x86.Port_IO.Inb (Register_Port);
   exception
      when Constraint_Error =>
         return 0;
   end Read_Byte_From_Register;

   ----------------------------------------------------------------------------
   --  Read_Word_From_Register
   ----------------------------------------------------------------------------
   function Read_Word_From_Register (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type
   ) return Unsigned_16 is
      Register_Port : System.Address;
   begin
      Register_Port := Get_Register_Address (Bus, Register);

      return x86.Port_IO.Inw (Register_Port);
   exception
      when Constraint_Error =>
         return 0;
   end Read_Word_From_Register;

   ----------------------------------------------------------------------------
   --  Write_Byte_To_Register
   ----------------------------------------------------------------------------
   procedure Write_Byte_To_Register (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type;
     Value    : Unsigned_8
   ) is
      Register_Port : System.Address;
   begin
      Register_Port := Get_Register_Address (Bus, Register);

      x86.Port_IO.Outb (Register_Port, Value);
   exception
      when Constraint_Error =>
         return;
   end Write_Byte_To_Register;

   ----------------------------------------------------------------------------
   --  Write_Word_To_Register
   ----------------------------------------------------------------------------
   procedure Write_Word_To_Register (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type;
     Value    : Unsigned_16
   ) is
      Register_Port : System.Address;
   begin
      Register_Port := Get_Register_Address (Bus, Register);

      x86.Port_IO.Outw (Register_Port, Value);
   exception
      when Constraint_Error =>
         return;
   end Write_Word_To_Register;

end x86.ATA;
