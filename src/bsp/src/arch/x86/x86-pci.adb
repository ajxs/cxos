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

with x86.Port_IO;
with System.Storage_Elements; use System.Storage_Elements;

package body x86.PCI is
   ----------------------------------------------------------------------------
   --  Pci_Read_Long
   ----------------------------------------------------------------------------
   function Pci_Read_Long (
     Output          : out Unsigned_32;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     PCI_Device_Number;
     Function_Number :     PCI_Function_Number;
     Offset          :     Unsigned_8
   ) return Process_Result is
      --  The PCI address register value to send to read
      --  the value back from.
      Register_Address : Pci_Config_Address;
   begin
      --  Offset addresses consecutive DWORDs in the PCI addres space, as such
      --  it must be a multiple of 4.
      if (Offset and 3) /= 0 then
         return Misaligned_Offset;
      end if;

      --  Configure the register address to read.
      Register_Address := (
        Offset          => Offset,
        Function_Number => Function_Number,
        Device_Number   => Device_Number,
        Bus_Number      => Bus_Number,
        Reserved        => False,
        Enable          => True
      );

      --  Set the address register.
      x86.Port_IO.Outl (To_Address (PCI_CONFIG_ADDRESS_PORT),
        Pci_Config_Address_To_Long (Register_Address));
      --  Read the data in.
      Output := x86.Port_IO.Inl (To_Address (PCI_CONFIG_DATA_PORT));

      return Success;
   end Pci_Read_Long;
end x86.PCI;
