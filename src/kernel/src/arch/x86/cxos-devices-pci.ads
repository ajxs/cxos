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
with x86.PCI;

-------------------------------------------------------------------------------
--  CXOS.DEVICES.PCI
--
--  Purpose:
--    This package contains code for implementing PCI bus drivers.
-------------------------------------------------------------------------------
package Cxos.Devices.PCI is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Find_Pci_Devices
   --
   --  Purpose:
   --    Searches the PCI Address space to find PCI devices.
   ----------------------------------------------------------------------------
   function Find_PCI_Devices return Process_Result
   with Volatile_Function;

private
   ----------------------------------------------------------------------------
   --  PCI Device Type
   ----------------------------------------------------------------------------
   type PCI_Device_T is
      record
         Bus_Number       : Unsigned_8;
         Device_Number    : x86.PCI.Pci_Device_Number;
         Function_Number  : x86.PCI.Pci_Function_Number;
         Vendor_Id        : Unsigned_16;
         Device_Id        : Unsigned_16;
         Command          : Unsigned_16;
         Status           : Unsigned_16;
         Revision_Id      : Unsigned_8;
         Prog_IF          : Unsigned_8;
         Subclass         : Unsigned_8;
         Device_Class     : Unsigned_8;
         Cache_Line_Size  : Unsigned_8;
         Latency_Timer    : Unsigned_8;
         Header_Type      : Unsigned_8;
         BIST             : Unsigned_8;
         BAR0             : Unsigned_32;
         BAR1             : Unsigned_32;
         BAR2             : Unsigned_32;
         BAR3             : Unsigned_32;
         BAR4             : Unsigned_32;
         BAR5             : Unsigned_32;
         Cardbus_CIS_Ptr  : Unsigned_32;
         Subsys_Vendor_Id : Unsigned_16;
         Subsys_Id        : Unsigned_16;
         Expansion_Base   : Unsigned_32;
         Capabilities_Ptr : Unsigned_8;
         Interrupt_Line   : Unsigned_8;
         Interrupt_Pin    : Unsigned_8;
         Min_Grant        : Unsigned_8;
         Max_Latency      : Unsigned_8;
      end record;

   ----------------------------------------------------------------------------
   --  Read_Pci_Device
   --
   --  Purpose:
   --    This function reads the PCI bus to collect information on a specific
   --    PCI device.
   ----------------------------------------------------------------------------
   function Read_PCI_Device (
     Device          : out PCI_Device_T;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     x86.PCI.Pci_Device_Number;
     Function_Number :     x86.PCI.Pci_Function_Number
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Test_Pci_Device
   --
   --  Purpose:
   --    This function tests whether a device is present on the PCI bus at a
   --    particular address. The function sets a boolean value indicating
   --    whether a valid device is present.
   ----------------------------------------------------------------------------
   function Test_PCI_Device (
     Result          : out Boolean;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     x86.PCI.Pci_Device_Number;
     Function_Number :     x86.PCI.Pci_Function_Number
   ) return Process_Result;

end Cxos.Devices.PCI;
