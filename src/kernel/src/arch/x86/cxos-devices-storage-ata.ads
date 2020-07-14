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

with Ada.Unchecked_Conversion;
with Cxos.Time_Keeping;
with x86.ATA; use x86.ATA;

-------------------------------------------------------------------------------
--  CXOS.DEVICES.STORAGE.ATA
--
--  Purpose:
--    This package contains definitons and functionality for working with
--    ATA devices.
-------------------------------------------------------------------------------
package Cxos.Devices.Storage.ATA is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  ATA read buffer type.
   --  This type is used as a destination buffer for reading from an ATA
   --  device. The actual theoretical maximum size is 16776960 words. This
   --  represents the maximum number of words that can be read with a 16bit
   --  sector count. Assuming that LBA48 mode was used.
   ----------------------------------------------------------------------------
   type ATA_Read_Buffer is array (Natural range <>) of Unsigned_16;

   ----------------------------------------------------------------------------
   --  ATA Device Type Record
   --
   --  Purpose:
   --    Describes an ATA device on the IDE controller.
   ----------------------------------------------------------------------------
   type ATA_Device is
      record
         Present        : Boolean;
         Bus            : ATA_Bus;
         Position       : ATA_Device_Position;
         Device_Type    : ATA_Device_Type;
         Identification : Device_Identification_Record;
      end record;

   ----------------------------------------------------------------------------
   --  ATA Device array type.
   ----------------------------------------------------------------------------
   type ATA_Device_Array is array (Natural range 0 .. 7) of ATA_Device;

   ----------------------------------------------------------------------------
   --  The ATA devices attached to the system.
   ----------------------------------------------------------------------------
   ATA_Devices : ATA_Device_Array;

   ----------------------------------------------------------------------------
   --  Find_ATA_Devices
   --
   --  Purpose:
   --    Populates the list of ATA devices supported by the system.
   ----------------------------------------------------------------------------
   procedure Find_ATA_Devices;

   ----------------------------------------------------------------------------
   --  Print_Identification_Record
   ----------------------------------------------------------------------------
   procedure Print_ATA_Device (
     Device : ATA_Device
   );

   ----------------------------------------------------------------------------
   --  Read_ATA_Device
   ----------------------------------------------------------------------------
   procedure Read_ATA_Device (
     Bus        :     x86.ATA.ATA_Bus;
     Position   :     x86.ATA.ATA_Device_Position;
     Sector_Cnt :     x86.ATA.ATA_Sector_Count;
     LBA        :     x86.ATA.ATA_LBA;
     Buffer     : out ATA_Read_Buffer;
     Status     : out Process_Result;
     Mode       :     x86.ATA.LBA_Mode := LBA28
   );

private
   ----------------------------------------------------------------------------
   --  Read_ATA_Device_Info
   ----------------------------------------------------------------------------
   procedure Read_ATA_Device_Info (
     Device   : out ATA_Device;
     Bus      :     ATA_Bus;
     Position :     ATA_Device_Position;
     Status   : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Identify
   --
   --  Purpose:
   --    Reads the identification buffer from a specific ATA device.
   ----------------------------------------------------------------------------
   function Identify (
     Id_Record : out x86.ATA.Device_Identification_Record;
     Bus       :     x86.ATA.ATA_Bus;
     Position  :     x86.ATA.ATA_Device_Position
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Identify_Packet_Device
   --
   --  Purpose:
   --    Reads the identification buffer from a specific ATAPI device.
   ----------------------------------------------------------------------------
   function Identify_Packet_Device (
     Id_Record : out x86.ATA.Device_Identification_Record;
     Bus       :     x86.ATA.ATA_Bus;
     Position  :     x86.ATA.ATA_Device_Position
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Get_Device_Type
   --
   --  Purpose:
   --    Returns the type of the specified ATA device.
   ----------------------------------------------------------------------------
   procedure Get_Device_Type (
     Device_Type : out x86.ATA.ATA_Device_Type;
     Bus         :     x86.ATA.ATA_Bus;
     Position    :     x86.ATA.ATA_Device_Position;
     Status      : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Reset_Bus
   --
   --  Purpose:
   --    Performs a software reset of an ATA device bus.
   ----------------------------------------------------------------------------
   procedure Reset_Bus (
     Bus    :     x86.ATA.ATA_Bus;
     Status : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Drive_Select_Delay
   --
   --  Purpose:
   --    Performs the necessary delay after a drive selection, as per ATA spec.
   ----------------------------------------------------------------------------
   procedure Drive_Select_Delay;

   ----------------------------------------------------------------------------
   --  Select_Device_Position
   --
   --  Purpose:
   --    Selects which device position (Master/Slave) is selected on a
   --    particular ATA bus.
   ----------------------------------------------------------------------------
   procedure Select_Device_Position (
     Bus      :     x86.ATA.ATA_Bus;
     Position :     x86.ATA.ATA_Device_Position;
     Status   : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Send_Command
   --
   --  Purpose:
   --    Sends a command to the currently selected device on an ATA bus.
   ----------------------------------------------------------------------------
   procedure Send_Command (
     Bus          :     x86.ATA.ATA_Bus;
     Command_Type :     x86.ATA.ATA_Command;
     Status       : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Wait_For_Device_Ready
   --
   --  Purpose:
   --    Polls the specified Bus/Device until it is ready to receive commands.
   --    Alternatively, this procedure can be used to poll until a device
   --    is ready to transfer data.
   --    A timeout value can be specified. This will dictate at what point to
   --    timeout and return a busy status.
   ----------------------------------------------------------------------------
   procedure Wait_For_Device_Ready (
     Bus           :     x86.ATA.ATA_Bus;
     Status        : out Process_Result;
     Timeout       :     Cxos.Time_Keeping.Time := 2000;
     Wait_For_Data :     Boolean := False
   );

   ----------------------------------------------------------------------------
   --  Flush_Bus_Write_Cache
   ----------------------------------------------------------------------------
   procedure Flush_Bus_Write_Cache (
     Bus    : x86.ATA.ATA_Bus;
     Status : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Print_Program_Status
   --
   --  Purpose:
   --    Returns a string representing a process result object.
   ----------------------------------------------------------------------------
   function Print_Program_Status (
     Result : Process_Result
   ) return String;

end Cxos.Devices.Storage.ATA;
