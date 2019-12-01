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
   type ATA_Regiser_Type is (
     Data,
     Error,
     Features,
     Sector_Count,
     Sector_Number,
     Cylinder_Low,
     Cylinder_High,
     Drive_Head,
     Device_Status,
     Command,
     Alt_Status,
     Device_Control,
     Drive_Address
   );

   ----------------------------------------------------------------------------
   --  Get_Device_Type
   --
   --  Purpose:
   --    Returns the type of the specified ATA device.
   ----------------------------------------------------------------------------
   function Get_Device_Type (
     Bus      : ATA_Bus;
     Position : ATA_Device_Position
   ) return ATA_Device_Type;

   ----------------------------------------------------------------------------
   --  Get_Register_Address
   --
   --  Purpose:
   --    Returns the port address of an individual ATA device register.
   ----------------------------------------------------------------------------
   function Get_Register_Address (
     Bus      : ATA_Bus;
     Register : ATA_Regiser_Type
   ) return System.Address
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Reset_Bus
   --
   --  Purpose:
   --    Performs a software reset of an ATA device bus.
   ----------------------------------------------------------------------------
   procedure Reset_Bus (
     Bus : ATA_Bus
   );

   ----------------------------------------------------------------------------
   --  Select_Device_Position
   --
   --  Purpose:
   --    Selects which device position (Master/Slave) is selected on a
   --    particular ATA bus.
   ----------------------------------------------------------------------------
   procedure Select_Device_Position (
     Bus      : ATA_Bus;
     Position : ATA_Device_Position
   );
end x86.ATA;
