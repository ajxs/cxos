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
with x86.PCI; use x86.PCI;

-------------------------------------------------------------------------------
--  CXOS.DEVICES.PCI
--
--  Purpose:
--    This package contains code for implementing PCI bus drivers.
-------------------------------------------------------------------------------
package Cxos.Devices.PCI is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Program status type.
   ----------------------------------------------------------------------------
   type Program_Status is (
     Bus_Read_Error,
     Invalid_Argument,
     Misaligned_Offset,
     Success,
     Unhandled_Exception
   );

   ----------------------------------------------------------------------------
   --  Query_PCI_Bus
   --
   --  Purpose:
   --    Enumerates the devices attached to the PCI bus.
   ----------------------------------------------------------------------------
   procedure Query_PCI_Bus (
     PCI_Bus : out Device_Bus_T;
     Status  : out Program_Status
   );

   ----------------------------------------------------------------------------
   --  Image_Status
   ----------------------------------------------------------------------------
   function Image_Status (Status : Program_Status) return String
   with Pure_Function;

private
   ----------------------------------------------------------------------------
   --  PCI Device Type
   ----------------------------------------------------------------------------
   type PCI_Device_T is
      record
         Bus_Number       : Unsigned_8;
         Device_Number    : x86.PCI.PCI_Device_Number;
         Function_Number  : x86.PCI.PCI_Function_Number;
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
   procedure Read_PCI_Device (
     Device          : out PCI_Device_T;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     x86.PCI.PCI_Device_Number;
     Function_Number :     x86.PCI.PCI_Function_Number;
     Status          : out Program_Status
   );

   ----------------------------------------------------------------------------
   --  Test_Pci_Device
   --
   --  Purpose:
   --    This function tests whether a device is present on the PCI bus at a
   --    particular address. The function sets a boolean value indicating
   --    whether a valid device is present.
   ----------------------------------------------------------------------------
   procedure Test_PCI_Device (
     Result          : out Boolean;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     x86.PCI.PCI_Device_Number;
     Function_Number :     x86.PCI.PCI_Function_Number;
     Status          : out Program_Status
   );

   ----------------------------------------------------------------------------
   --  Read_Long
   --
   --  Purpose:
   --    This function reads an unsigned long from the PCI bus.
   ----------------------------------------------------------------------------
   procedure Read_Long (
     Output          : out Unsigned_32;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     PCI_Device_Number;
     Function_Number :     PCI_Function_Number;
     Offset          :     Unsigned_8;
     Status          : out Program_Status
   );

end Cxos.Devices.PCI;
