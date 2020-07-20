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
--  CXOS.DEVICES
--
--  Purpose:
--    This package contains definitons and functionality for working with
--    system devices.
-------------------------------------------------------------------------------
package Cxos.Devices is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Process Result type.
   --  Tracks the outcome of internal functions.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Bus_Read_Error,
     Command_Aborted,
     Device_Busy,
     Device_In_Error_State,
     Device_Non_ATA,
     Device_Not_Found,
     Device_Not_Present,
     Device_Read_Buffer_Overflow,
     Drive_Fault,
     Invalid_Command,
     Packet_Interface_Not_Supported,
     Success,
     Unhandled_Exception
   );

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type Device_Bus_Type_T is (
     Device_Bus_Type_PCI
   );

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type Device_Bus_T is
      record
         Device_Bus_Type : Device_Bus_Type_T;
      end record;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise;

end Cxos.Devices;
