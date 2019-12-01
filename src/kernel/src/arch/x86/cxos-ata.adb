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

with Cxos.Serial;
with Interfaces;
with System;
with x86.Port_IO;

package body Cxos.ATA is
   use Interfaces;
   use x86.ATA;

   ----------------------------------------------------------------------------
   --  Get_Device_Type
   ----------------------------------------------------------------------------
   function Get_Device_Type (
     Device_Type : out x86.ATA.ATA_Device_Type;
     Bus         :     x86.ATA.ATA_Bus;
     Position    :     x86.ATA.ATA_Device_Position
   ) return Process_Result is
      --  The result of internal processes.
      Result             : Process_Result;
      Cylinder_High_Port : System.Address;
      Cylinder_Low_Port  : System.Address;

      Drive_Cylinder_Low  : Unsigned_8;
      Drive_Cylinder_High : Unsigned_8;
   begin
      --  Select the master/slave device.
      Result := Select_Device_Position (Bus, Position);
      if Result /= Success then
         return Result;
      end if;

      --  Get the device port addresses used.
      Cylinder_Low_Port  := Get_Register_Address (Bus, Cylinder_Low);
      Cylinder_High_Port := Get_Register_Address (Bus, Cylinder_High);

      --  Wait until the device is ready to receive commands.
      Result := Wait_For_Device_Ready (Bus, 10000);
      if Result /= Success then
         return Result;
      end if;

      --  Read device identification info.
      Drive_Cylinder_High := x86.Port_IO.Inb (Cylinder_High_Port);
      Drive_Cylinder_Low  := x86.Port_IO.Inb (Cylinder_Low_Port);

      if Drive_Cylinder_Low = 16#14# and Drive_Cylinder_High = 16#EB# then
         Device_Type := PATAPI;
      elsif Drive_Cylinder_Low = 16#69# and Drive_Cylinder_High = 16#96# then
         Device_Type := SATAPI;
      elsif Drive_Cylinder_Low = 16#3C# and Drive_Cylinder_High = 16#C3# then
         Device_Type := SATA;
      elsif Drive_Cylinder_Low = 0 and Drive_Cylinder_High = 0 then
         Device_Type := PATA;
      else
         Device_Type := Unknown_ATA_Device;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Get_Device_Type;

   procedure Initialise is
      Device_Type : x86.ATA.ATA_Device_Type;
      Result      : Process_Result;
   begin
      Result := Reset_Bus (Primary);
      if Result /= Success then
         Cxos.Serial.Put_String ("Error resetting device" & ASCII.LF);
      end if;

      Result := Get_Device_Type (Device_Type, Primary, Master);
      if Result /= Success then
         Cxos.Serial.Put_String ("Error reading device type" & ASCII.LF);
      end if;

      case Device_Type is
         when PATAPI =>
            Cxos.Serial.Put_String ("PATAPI" & ASCII.LF);
         when SATAPI =>
            Cxos.Serial.Put_String ("PATAPI" & ASCII.LF);
         when PATA   =>
            Cxos.Serial.Put_String ("PATA" & ASCII.LF);
         when SATA   =>
            Cxos.Serial.Put_String ("SATA" & ASCII.LF);
         when Unknown_ATA_Device =>
            Cxos.Serial.Put_String ("Unknown" & ASCII.LF);
      end case;
   exception
      when Constraint_Error =>
         return;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Reset_Bus
   ----------------------------------------------------------------------------
   function Reset_Bus (
     Bus : x86.ATA.ATA_Bus
   ) return Process_Result is
      --  The address of the device control register.
      Control_Register_Address : System.Address;
   begin
      Control_Register_Address := x86.ATA.Get_Register_Address (Bus,
        Device_Control);

      x86.Port_IO.Outb (Control_Register_Address, 4);
      x86.Port_IO.Outb (Control_Register_Address, 0);

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Reset_Bus;

   ----------------------------------------------------------------------------
   --  Select_Device_Position
   ----------------------------------------------------------------------------
   function Select_Device_Position (
     Bus      : x86.ATA.ATA_Bus;
     Position : x86.ATA.ATA_Device_Position
   ) return Process_Result is
      --  The address of the Device Select port.
      Device_Select_Port : System.Address;
   begin
      --  Get the Device Select Port for this device.
      Get_Device_Select_Port :
         begin
            Device_Select_Port := x86.ATA.Get_Register_Address (Bus,
              Drive_Head);
         exception
            when Constraint_Error =>
               return Unhandled_Exception;
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
               return Unhandled_Exception;
         end Send_Device_Select_Signal;

      return Success;
   end Select_Device_Position;

   ----------------------------------------------------------------------------
   --  Wait_For_Device_Ready
   ----------------------------------------------------------------------------
   function Wait_For_Device_Ready (
     Bus           : x86.ATA.ATA_Bus;
     Attempt_Count : Integer := 2000
   ) return Process_Result is
      --  The address of the device alt status port.
      Alt_Status_Port : System.Address;
      --  The status value read from the device.
      Drive_Status    : Unsigned_8;
   begin
      --  Get the port address of the alt status register.
      Alt_Status_Port := Get_Register_Address (Bus, Alt_Status);

      --  Read the device status register in a loop until either the
      --  attempt count is exceeded and the function times out, or a non-busy
      --  status is read.
      Wait_While_Busy :
         for I in Integer range 0 .. Attempt_Count loop
            --  Read device status.
            Drive_Status := x86.Port_IO.Inb (Alt_Status_Port);
            if (Drive_Status and 16#80#) = 0 then
               return Success;
            end if;
         end loop Wait_While_Busy;

      --  If no value has been returned within the attempt threshold,
      --  return this status.
      return Device_Busy;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Wait_For_Device_Ready;
end Cxos.ATA;
