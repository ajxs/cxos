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
with Interfaces;
with Cxos.Time_Keeping;
with x86.ATA;

-------------------------------------------------------------------------------
--  CXOS.ATA
--
--  Purpose:
--    This package contains definitons and functionality for working with
--    ATA devices.
-------------------------------------------------------------------------------
package Cxos.ATA is
   pragma Preelaborate;

   use Interfaces;

   ----------------------------------------------------------------------------
   --  Process Result type.
   --  Used for tracking the result of package processes.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Command_Aborted,
     Device_Busy,
     Device_Error_State,
     Device_Non_ATA,
     Device_Not_Present,
     Invalid_Command,
     Success,
     Unhandled_Exception
   );

   procedure Initialise;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Read_Word (
     Data : out Unsigned_16;
     Bus  :     x86.ATA.ATA_Bus
   ) return Process_Result;

private
   ----------------------------------------------------------------------------
   --  Device Identification Buffer type.
   --  Used for reading the identification record from a device.
   ----------------------------------------------------------------------------
   type Device_Identification_Buffer is
     array (Integer range 0 .. 255) of Unsigned_16;

   ----------------------------------------------------------------------------
   --  Device_Identification_Buffer_To_Record
   --
   --  Purpose:
   --    Converts a device identification buffer to the device identification
   --    record type.
   ----------------------------------------------------------------------------
   function Device_Identification_Buffer_To_Record is
      new Ada.Unchecked_Conversion (
        Source => Device_Identification_Buffer,
        Target => x86.ATA.Device_Identification_Record
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
   --  Get_Device_Type
   --
   --  Purpose:
   --    Returns the type of the specified ATA device.
   ----------------------------------------------------------------------------
   function Get_Device_Type (
     Device_Type : out x86.ATA.ATA_Device_Type;
     Bus         :     x86.ATA.ATA_Bus;
     Position    :     x86.ATA.ATA_Device_Position
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Reset_Bus
   --
   --  Purpose:
   --    Performs a software reset of an ATA device bus.
   ----------------------------------------------------------------------------
   function Reset_Bus (
     Bus : x86.ATA.ATA_Bus
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Select_Device_Position
   --
   --  Purpose:
   --    Selects which device position (Master/Slave) is selected on a
   --    particular ATA bus.
   ----------------------------------------------------------------------------
   function Select_Device_Position (
     Bus      : x86.ATA.ATA_Bus;
     Position : x86.ATA.ATA_Device_Position
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Send_Command
   --
   --  Purpose:
   --    Sends a command to the currently selected device on an ATA bus.
   ----------------------------------------------------------------------------
   function Send_Command (
     Bus          : x86.ATA.ATA_Bus;
     Command_Type : x86.ATA.ATA_Command
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Wait_For_Device_Ready
   --
   --  Purpose:
   --    Waits until the specified Bus/Device is ready to receive commands.
   ----------------------------------------------------------------------------
   function Wait_For_Device_Ready (
     Bus     : x86.ATA.ATA_Bus;
     Timeout : Cxos.Time_Keeping.Time := 2000
   ) return Process_Result
   with Volatile_Function;
end Cxos.ATA;
