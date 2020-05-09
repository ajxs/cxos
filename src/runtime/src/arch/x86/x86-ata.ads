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
     Sector_Count_Reg,
     Sector_Number_Reg,
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
   --  ATA Sector Count type.
   ----------------------------------------------------------------------------
   subtype ATA_Sector_Count is Unsigned_16;

   ----------------------------------------------------------------------------
   --  ATA_LBA type.
   ----------------------------------------------------------------------------
   type ATA_LBA is mod 2 ** 48;

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
   --  Device Error record type.
   --  Contains various flags related to the error status of an ATA device.
   ----------------------------------------------------------------------------
   type Device_Error_Record is
      record
         AMNF  : Boolean;
         TKZNF : Boolean;
         ABRT  : Boolean;
         MCR   : Boolean;
         IDNF  : Boolean;
         MC    : Boolean;
         UNC   : Boolean;
         BBK   : Boolean;
      end record
   with Size => 8;
   for Device_Error_Record use
      record
         AMNF  at 0 range 0 .. 0;
         TKZNF at 0 range 1 .. 1;
         ABRT  at 0 range 2 .. 2;
         MCR   at 0 range 3 .. 3;
         IDNF  at 0 range 4 .. 4;
         MC    at 0 range 5 .. 5;
         UNC   at 0 range 6 .. 6;
         BBK   at 0 range 7 .. 7;
      end record;

   ----------------------------------------------------------------------------
   --  Unsigned_8_To_Device_Error_Record
   --
   --  Purpose:
   --    Converts an unsigned byte to an ATA error status record.
   ----------------------------------------------------------------------------
   function Unsigned_8_To_Device_Error_Record is
      new Ada.Unchecked_Conversion (
        Source => Unsigned_8,
        Target => Device_Error_Record
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
     Identify_Packet_Device,
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
   --  ATA Device Command set support type.
   --  Represents the command sets supported by a device.
   ----------------------------------------------------------------------------
   type Device_Command_Set_Support_Record is
      record
         DOWNLOAD_MICROCODE                  : Boolean;
         R_W_DMA_QUEUED                      : Boolean;
         CFA                                 : Boolean;
         Advanced_Power_Management           : Boolean;
         Removable_Media_Status_Notification : Boolean;
         Power_Up_In_Standby                 : Boolean;
         SET_FEATURES                        : Boolean;
         Address_Offset_Reserved             : Boolean;
         SET_MAX                             : Boolean;
         Automatic_Acoustic_Management       : Boolean;
         LBA48                               : Boolean;
         Device_Configuration_Overlay        : Boolean;
         FLUSH_CACHE                         : Boolean;
         FLUSH_CACHE_EXT                     : Boolean;
         Set_To_One                          : Boolean;
         Set_To_Zero                         : Boolean;
      end record
   with Size => 16;
   for Device_Command_Set_Support_Record use
      record
         DOWNLOAD_MICROCODE                  at 0 range 0  .. 0;
         R_W_DMA_QUEUED                      at 0 range 1  .. 1;
         CFA                                 at 0 range 2  .. 2;
         Advanced_Power_Management           at 0 range 3  .. 3;
         Removable_Media_Status_Notification at 0 range 4  .. 4;
         Power_Up_In_Standby                 at 0 range 5  .. 5;
         SET_FEATURES                        at 0 range 6  .. 6;
         Address_Offset_Reserved             at 0 range 7  .. 7;
         SET_MAX                             at 0 range 8  .. 8;
         Automatic_Acoustic_Management       at 0 range 9  .. 9;
         LBA48                               at 0 range 10 .. 10;
         Device_Configuration_Overlay        at 0 range 11 .. 11;
         FLUSH_CACHE                         at 0 range 12 .. 12;
         FLUSH_CACHE_EXT                     at 0 range 13 .. 13;
         Set_To_One                          at 0 range 14 .. 14;
         Set_To_Zero                         at 0 range 15 .. 15;
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
         Command_Set_Support   : Device_Command_Set_Support_Record;
      end record
   with Size => 16#1000#;
   for Device_Identification_Record use
      record
         Device_Config         at 0   range 0 .. 15;
         Specific_Config       at 2   range 0 .. 15;
         Serial_Number         at 20  range 0 .. 159;
         Firmware_Revision     at 46  range 0 .. 63;
         Model_Number          at 54  range 0 .. 319;
         Field_1               at 94  range 0 .. 15;
         Capabilities_1        at 98  range 0 .. 15;
         Capabilities_2        at 100 range 0 .. 15;
         Total_Logical_Sectors at 120 range 0 .. 15;
         Command_Set_Support   at 166 range 0 .. 15;
      end record;
   pragma Warnings (On);

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
        Target => Device_Identification_Record
      );

end x86.ATA;
