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

with Interfaces;
with System.Storage_Elements;
with x86.Port_IO;

package body x86.ATA is
   use Interfaces;
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Get_Device_Type
   ----------------------------------------------------------------------------
   function Get_Device_Type (
     Bus      : ATA_Bus;
     Position : ATA_Device_Position
   ) return ATA_Device_Type is
      Cylinder_High_Port : System.Address;
      Cylinder_Low_Port  : System.Address;
      Alt_Status_Port    : System.Address;

      pragma Warnings (Off);

      Drive_Status        : Unsigned_8;
      Drive_Cylinder_Low  : Unsigned_8;
      Drive_Cylinder_High : Unsigned_8;
   begin
      Select_Device_Position (Bus, Position);

      Cylinder_Low_Port  := Get_Register_Address (Bus, Cylinder_Low);
      Cylinder_High_Port := Get_Register_Address (Bus, Cylinder_High);
      Alt_Status_Port    := Get_Register_Address (Bus, Alt_Status);

      Drive_Status := x86.Port_IO.Inb (Alt_Status_Port);
      Drive_Status := x86.Port_IO.Inb (Alt_Status_Port);
      Drive_Status := x86.Port_IO.Inb (Alt_Status_Port);
      Drive_Status := x86.Port_IO.Inb (Alt_Status_Port);
      Drive_Status := x86.Port_IO.Inb (Alt_Status_Port);
      pragma Warnings (On);

      Drive_Cylinder_High := x86.Port_IO.Inb (Cylinder_High_Port);
      Drive_Cylinder_Low  := x86.Port_IO.Inb (Cylinder_Low_Port);

      if Drive_Cylinder_Low = 16#14# and Drive_Cylinder_High = 16#EB# then
         return PATAPI;
      elsif Drive_Cylinder_Low = 16#69# and Drive_Cylinder_High = 16#96# then
         return SATAPI;
      elsif Drive_Cylinder_Low = 16#3C# and Drive_Cylinder_High = 16#C3# then
         return SATA;
      elsif Drive_Cylinder_Low = 0 and Drive_Cylinder_High = 0 then
         return PATA;
      end if;

      return Unknown_ATA_Device;
   exception
      when Constraint_Error =>
         return Unknown_ATA_Device;
   end Get_Device_Type;

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

   ----------------------------------------------------------------------------
   --  Reset_Bus
   ----------------------------------------------------------------------------
   procedure Reset_Bus (
     Bus : ATA_Bus
   ) is
      --  The address of the device control register.
      Control_Register_Address : System.Address;
   begin
      Control_Register_Address := Get_Register_Address (Bus, Device_Control);

      x86.Port_IO.Outb (Control_Register_Address, 4);
      x86.Port_IO.Outb (Control_Register_Address, 0);
   exception
      when Constraint_Error =>
         return;
   end Reset_Bus;

   ----------------------------------------------------------------------------
   --  Select_Device_Position
   ----------------------------------------------------------------------------
   procedure Select_Device_Position (
     Bus      : ATA_Bus;
     Position : ATA_Device_Position
   ) is
      Device_Select_Port : System.Address;
   begin
      --  Get the Device Select Port for this device.
      Get_Device_Select_Port :
         begin
            Device_Select_Port := Get_Register_Address (Bus, Drive_Head);
         exception
            when Constraint_Error =>
               return;
         end Get_Device_Select_Port;

      --  Sends the signal to select the specified device position.
      Send_Device_Select_Signal :
         begin
            case Position is
               when Master =>
                  x86.Port_IO.Outb (Device_Select_Port, 16#A0#);
               when Slave  =>
                  x86.Port_IO.Outb (Device_Select_Port, 16#B0#);
            end case;
         exception
            when Constraint_Error =>
               return;
         end Send_Device_Select_Signal;
   end Select_Device_Position;

end x86.ATA;
