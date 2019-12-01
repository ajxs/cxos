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

   ----------------------------------------------------------------------------
   --  Process Result type.
   --  Used for tracking the result of package processes.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Success,
     Unhandled_Exception
   );

   procedure Initialise;

private
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

end Cxos.ATA;
