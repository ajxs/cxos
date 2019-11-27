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

-------------------------------------------------------------------------------
--  X86.PCI
--
--  Purpose:
--    This package contains code and defintions for implementing and working
--    with the system's PCI bus.
-------------------------------------------------------------------------------
package x86.PCI is
   pragma Preelaborate (x86.PCI);

   use Interfaces;

   ----------------------------------------------------------------------------
   --  Result type to track the outcome of procedures.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Invalid_Argument,
     Misaligned_Offset,
     Success
   );

   ----------------------------------------------------------------------------
   --  TODO:
   ----------------------------------------------------------------------------
   type Pci_Function_Number is mod 2 ** 3;

   ----------------------------------------------------------------------------
   --  TODO:
   ----------------------------------------------------------------------------
   type Pci_Device_Number   is mod 2 ** 5;

   ----------------------------------------------------------------------------
   --  Pci_Read_Long
   --
   --  Purpose:
   --    This function reads an unsigned long from the PCI bus.
   ----------------------------------------------------------------------------
   function Pci_Read_Long (
     Output          : out Unsigned_32;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     Pci_Device_Number;
     Function_Number :     Pci_Function_Number;
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
         Function_Number : Pci_Function_Number;
         Device_Number   : Pci_Device_Number;
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
