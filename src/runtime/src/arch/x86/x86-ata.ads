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

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

-------------------------------------------------------------------------------
--  X86.ATA
--
--  Purpose:
--    This package contains definitons and functionality for working with
--    ATA devices.
-------------------------------------------------------------------------------
package x86.ATA is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  ATA Device Bus type.
   ----------------------------------------------------------------------------
   type ATA_Bus is (
     Primary,
     Secondary
   );

   ----------------------------------------------------------------------------
   --  ATA Device Position type.
   --  Used for selecting what position a device occupies on the bus.
   ----------------------------------------------------------------------------
   type ATA_Device_Position is (
     Master,
     Slave
   );

   ----------------------------------------------------------------------------
   --  ATA Device type.
   --  Indicates the type of the ATA device.
   ----------------------------------------------------------------------------
   type ATA_Device_Type is (
     PATAPI,
     SATAPI,
     PATA,
     SATA,
     Unknown_ATA_Device
   );

   ----------------------------------------------------------------------------
   --  ATA Register type.
   --  Denotes individual ATA registers for a device.
   ----------------------------------------------------------------------------
   type ATA_Register_Type is (
     Data_Reg,
     Error_Reg,
     Features,
     Sector_Count,
     Sector_Number,
     Cylinder_Low,
     Cylinder_High,
     Drive_Head,
     Device_Status,
     Command_Reg,
     Alt_Status,
     Device_Control,
     Drive_Address
   );

   ----------------------------------------------------------------------------
   --  Device Status record type.
   --  Contains various flags related to the status of an ATA device.
   ----------------------------------------------------------------------------
   type Device_Status_Record is
      record
         ERR  : Boolean;
         IDX  : Boolean;
         CORR : Boolean;
         DRQ  : Boolean;
         SRV  : Boolean;
         DF   : Boolean;
         RDY  : Boolean;
         BSY  : Boolean;
      end record
   with Size => 8;
   for Device_Status_Record use
      record
         ERR  at 0 range 0 .. 0;
         IDX  at 0 range 1 .. 1;
         CORR at 0 range 2 .. 2;
         DRQ  at 0 range 3 .. 3;
         SRV  at 0 range 4 .. 4;
         DF   at 0 range 5 .. 5;
         RDY  at 0 range 6 .. 6;
         BSY  at 0 range 7 .. 7;
      end record;

   ----------------------------------------------------------------------------
   --  Unsigned_8_To_Device_Status_Record
   --
   --  Purpose:
   --    Converts an unsigned byte to an ATA device status record.
   ----------------------------------------------------------------------------
   function Unsigned_8_To_Device_Status_Record is
      new Ada.Unchecked_Conversion (
        Source => Unsigned_8,
        Target => Device_Status_Record
      );

   ----------------------------------------------------------------------------
   --  ATA Command type.
   --  The inidividual ATA commands supported.
   ----------------------------------------------------------------------------
   type ATA_Command is (
     Nop,
     Device_Reset,
     Flush_Write_Cache,
     Identify_Device,
     Recalibrate,
     Read_Sectors_Retry,
     Read_Sectors_No_Retry,
     Read_Long_Retry,
     Read_Long_No_Retry,
     Read_Sectors_Ext,
     Read_DMA_Ext,
     Write_Sectors_Retry,
     Write_Sectors_No_Retry,
     Write_Long_Retry,
     Write_Long_No_Retry
   );

   ----------------------------------------------------------------------------
   --  Read_Byte_From_Register
   --
   --  Purpose:
   --    Reads a byte from a specific ATA register.
   ----------------------------------------------------------------------------
   function Read_Byte_From_Register (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type
   ) return Unsigned_8
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Read_Word_From_Register
   --
   --  Purpose:
   --    Reads a word from a specific ATA register.
   ----------------------------------------------------------------------------
   function Read_Word_From_Register (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type
   ) return Unsigned_16
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Write_Byte_To_Regsiter
   --
   --  Purpose:
   --    Writes a byte to a specific ATA register.
   ----------------------------------------------------------------------------
   procedure Write_Byte_To_Register (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type;
     Value    : Unsigned_8
   );

   ----------------------------------------------------------------------------
   --  Write_Word_To_Register
   --
   --  Purpose:
   --    Writes a word to a specific ATA register.
   ----------------------------------------------------------------------------
   procedure Write_Word_To_Register (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type;
     Value    : Unsigned_16
   );

   ----------------------------------------------------------------------------
   --  Get_Register_Address
   --
   --  Purpose:
   --    Returns the port address of an individual ATA device register.
   ----------------------------------------------------------------------------
   function Get_Register_Address (
     Bus      : ATA_Bus;
     Register : ATA_Register_Type
   ) return System.Address
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  ATA Device Serial Number type.
   ----------------------------------------------------------------------------
   type Device_Serial_Number is
     array (Integer range 0 .. 19) of Character;

   ----------------------------------------------------------------------------
   --  ATA Device Firmware Revision type.
   ----------------------------------------------------------------------------
   type Device_Firmware_Revision is
     array (Integer range 0 .. 7) of Character;

   ----------------------------------------------------------------------------
   --  ATA Device Model Number type.
   ----------------------------------------------------------------------------
   type Device_Model_Number is
     array (Integer range 0 .. 39) of Character;

   ----------------------------------------------------------------------------
   --  ATA Device General Config type.
   --  Contains configuration information for an ATA device.
   ----------------------------------------------------------------------------
   type ATA_General_Device_Config is
      record
         Reserved            : Boolean;
         Retired             : Boolean;
         Response_Incomplete : Boolean;
         Retired_2           : Boolean;
         Obsolete            : Boolean;
         Removable_Media     : Boolean;
         Retired_3           : Boolean;
         ATA_Device          : Boolean;
      end record
   with Size => 16;
   for ATA_General_Device_Config use
      record
         Reserved            at 0 range 0  .. 0;
         Retired             at 0 range 1  .. 1;
         Response_Incomplete at 0 range 2  .. 2;
         Retired_2           at 0 range 3  .. 5;
         Obsolete            at 0 range 6  .. 6;
         Removable_Media     at 0 range 7  .. 7;
         Retired_3           at 0 range 8  .. 14;
         ATA_Device          at 0 range 15 .. 15;
      end record;

   ----------------------------------------------------------------------------
   --  ATA Device Identification Record type.
   --  Contains identification regarding the manufacturer and capabilities
   --  of an individual ATA device.
   --  Note: Warnings have been disabled for this record type to avoid
   --  build errors resulting from gaps in this record.
   ----------------------------------------------------------------------------
   pragma Warnings (Off);
   type Device_Identification_Record is
      record
         Device_Config         : ATA_General_Device_Config;
         Specific_Config       : Unsigned_16;
         Serial_Number         : Device_Serial_Number;
         Firmware_Revision     : Device_Firmware_Revision;
         Model_Number          : Device_Model_Number;
         Field_1               : Unsigned_16;
         Capabilities_1        : Unsigned_16;
         Capabilities_2        : Unsigned_16;
         Total_Logical_Sectors : Unsigned_16;
      end record
   with Size => 16#1000#;
   for Device_Identification_Record use
      record
         Device_Config         at 0   range 0 .. 15;
         Specific_Config       at 4   range 0 .. 15;
         Serial_Number         at 20  range 0 .. 159;
         Firmware_Revision     at 46  range 0 .. 63;
         Model_Number          at 54  range 0 .. 319;
         Field_1               at 94  range 0 .. 15;
         Capabilities_1        at 98  range 0 .. 15;
         Capabilities_2        at 100 range 0 .. 15;
         Total_Logical_Sectors at 120 range 0 .. 15;
      end record;
   pragma Warnings (On);
end x86.ATA;
