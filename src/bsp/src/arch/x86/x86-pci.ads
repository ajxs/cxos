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
with Interfaces; use Interfaces;

-------------------------------------------------------------------------------
--  X86.PCI
--
--  Purpose:
--    This package contains code and defintions for implementing and working
--    with the system's PCI bus.
-------------------------------------------------------------------------------
package x86.PCI is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Result type to track the outcome of procedures.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Invalid_Argument,
     Misaligned_Offset,
     Success
   );

   ----------------------------------------------------------------------------
   --  PCI Device function number type.
   ----------------------------------------------------------------------------
   type PCI_Function_Number is mod 2 ** 3;

   ----------------------------------------------------------------------------
   --  PCI device number type.
   ----------------------------------------------------------------------------
   type PCI_Device_Number   is mod 2 ** 5;

   ----------------------------------------------------------------------------
   --  PCI Command Register Fields.
   --  The format of PCI commands to be sent to a device.
   ----------------------------------------------------------------------------
   type Pci_Command is
      record
         IO_Space                 : Boolean;
         Memory_Space             : Boolean;
         Bus_Master               : Boolean;
         Special_Cycle            : Boolean;
         Memory_Write_Inv_Enable  : Boolean;
         VGA_Palette_Snoop        : Boolean;
         Parity_Error_Response    : Boolean;
         Reserved                 : Boolean := False;
         SERR_Enable              : Boolean;
         Fast_Back_To_Back_Enable : Boolean;
         Interrupt_Disable        : Boolean;
         Reserved_2               : Boolean := False;
      end record
   with Size => 16;
   for Pci_Command use
      record
         IO_Space                 at 0 range 0  .. 0;
         Memory_Space             at 0 range 1  .. 1;
         Bus_Master               at 0 range 2  .. 2;
         Special_Cycle            at 0 range 3  .. 3;
         Memory_Write_Inv_Enable  at 0 range 4  .. 4;
         VGA_Palette_Snoop        at 0 range 5  .. 5;
         Parity_Error_Response    at 0 range 6  .. 6;
         Reserved                 at 0 range 7  .. 7;
         SERR_Enable              at 0 range 8  .. 8;
         Fast_Back_To_Back_Enable at 0 range 9  .. 9;
         Interrupt_Disable        at 0 range 10 .. 10;
         Reserved_2               at 0 range 11 .. 15;
      end record;

   ----------------------------------------------------------------------------
   --  Pci_Command_To_Word
   --
   --  Purpose:
   --    Unchecked conversion to convert from a PCI device command to a 16 bit
   --    unsigned integer.
   ----------------------------------------------------------------------------
   function Pci_Command_To_Word is
      new Ada.Unchecked_Conversion (
        Source => Pci_Command,
        Target => Unsigned_16
      );

   ----------------------------------------------------------------------------
   --  Word_To_Pci_Command
   --
   --  Purpose:
   --    Unchecked conversion between a 16 bit unsigned interger and a PCI
   --    command.
   ----------------------------------------------------------------------------
   function Word_To_Pci_Command is
      new Ada.Unchecked_Conversion (
        Source => Unsigned_16,
        Target => Pci_Command
      );

   ----------------------------------------------------------------------------
   --  PCI Status Register Fields.
   --  The format of PCI status received from a device.
   ----------------------------------------------------------------------------
   type Pci_Status is
      record
         Reserved                  : Boolean := False;
         Interrupt_Status          : Boolean;
         Captabilities_List        : Boolean;
         Compatible_66Mhz          : Boolean;
         Reserved_2                : Boolean := False;
         Fast_Back_To_Back_Capable : Boolean;
         Master_Data_Parity_Error  : Boolean;
         DEVSEL_Timing             : Boolean;
         Signaled_Target_Abort     : Boolean;
         Received_Target_Abort     : Boolean;
         Received_Master_Abort     : Boolean;
         Signaled_System_Error     : Boolean;
         Detected_Parity_Error     : Boolean;
      end record
   with Size => 16;
   for Pci_Status use
      record
         Reserved                  at 0 range 0  .. 2;
         Interrupt_Status          at 0 range 3  .. 3;
         Captabilities_List        at 0 range 4  .. 4;
         Compatible_66Mhz          at 0 range 5  .. 5;
         Reserved_2                at 0 range 6  .. 6;
         Fast_Back_To_Back_Capable at 0 range 7  .. 7;
         Master_Data_Parity_Error  at 0 range 8  .. 8;
         DEVSEL_Timing             at 0 range 9  .. 10;
         Signaled_Target_Abort     at 0 range 11 .. 11;
         Received_Target_Abort     at 0 range 12 .. 12;
         Received_Master_Abort     at 0 range 13 .. 13;
         Signaled_System_Error     at 0 range 14 .. 14;
         Detected_Parity_Error     at 0 range 15 .. 15;
      end record;

   ----------------------------------------------------------------------------
   --  Pci_Status_To_Word
   --
   --  Purpose:
   --    Unchecked conversion to convert from a PCI device status to a 16 bit
   --    unsigned integer.
   ----------------------------------------------------------------------------
   function Pci_Status_To_Word is
      new Ada.Unchecked_Conversion (
        Source => Pci_Status,
        Target => Unsigned_16
      );

   ----------------------------------------------------------------------------
   --  Word_To_Pci_Status
   --
   --  Purpose:
   --    Unchecked conversion between a 16 bit unsigned interger and a PCI
   --    status.
   ----------------------------------------------------------------------------
   function Word_To_Pci_Status is
      new Ada.Unchecked_Conversion (
        Source => Unsigned_16,
        Target => Pci_Status
      );

   ----------------------------------------------------------------------------
   --  Pci_Read_Long
   --
   --  Purpose:
   --    This function reads an unsigned long from the PCI bus.
   ----------------------------------------------------------------------------
   function Pci_Read_Long (
     Output          : out Unsigned_32;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     PCI_Device_Number;
     Function_Number :     PCI_Function_Number;
     Offset          :     Unsigned_8
   ) return Process_Result
   with Volatile_Function;

private
   ----------------------------------------------------------------------------
   --  Config and data port addresses.
   ----------------------------------------------------------------------------
   PCI_CONFIG_ADDRESS_PORT : constant := 16#CF8#;
   PCI_CONFIG_DATA_PORT    : constant := 16#CFC#;

   ----------------------------------------------------------------------------
   --  TODO:
   ----------------------------------------------------------------------------
   type Pci_Config_Address is
      record
         Offset          : Unsigned_8;
         Function_Number : PCI_Function_Number;
         Device_Number   : PCI_Device_Number;
         Bus_Number      : Unsigned_8;
         Reserved        : Boolean := False;
         Enable          : Boolean;
      end record;
   for Pci_Config_Address'Size use 32;
   for Pci_Config_Address use
      record
         Offset          at 0 range 0  .. 7;
         Function_Number at 0 range 8  .. 10;
         Device_Number   at 0 range 11 .. 15;
         Bus_Number      at 0 range 16 .. 23;
         Reserved        at 0 range 24 .. 30;
         Enable          at 0 range 31 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  Config_Address_To_Long
   --
   --  Purpose:
   --    Unchecked conversion to convert from a PCI config address to a
   --    32bit unsigned integer.
   ----------------------------------------------------------------------------
   function Pci_Config_Address_To_Long is
      new Ada.Unchecked_Conversion (
        Source => Pci_Config_Address,
        Target => Unsigned_32
      );

end x86.PCI;
