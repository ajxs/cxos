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
with x86.Serial; use x86.Serial;
with System; use System;

-------------------------------------------------------------------------------
--  CXOS.DEVICES.SERIAL
--
--  Purpose:
--    This package contains functionality for interfacing with the target
--    architecture's serial hardware.
-------------------------------------------------------------------------------
package Cxos.Devices.Serial is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises a particular serial port.
   --    This function will configure the baud rate, word length, parity
   --    and stop bits for a particular serial port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise (
     Port : Serial_Port;
     Rate : Baud_Rate := MAXIMUM_BAUD_RATE
   );

   ----------------------------------------------------------------------------
   --  Put_String
   --
   --  Purpose:
   --    This procedure prints a string to the default output serial port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Put_String (
     Data : String
   );

   ----------------------------------------------------------------------------
   --  Put_String
   --
   --  Purpose:
   --    This procedure prints a string to the selected serial port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Put_String (
     Port : Serial_Port;
     Data : String
   );

private
   ----------------------------------------------------------------------------
   --  Get_Port_Address
   --
   --  Purpose:
   --    This function returns the port-mapped address of an individual serial
   --    port. It will return the address of COM1 in the event of any error.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Get_Port_Address (
     Port : Serial_Port
   ) return System.Address
   with Pure_Function,
     Inline;

   ----------------------------------------------------------------------------
   --  Get_Register_Address
   --
   --  Purpose:
   --    This function returns the port-mapped address of an individual
   --    serial port register.
   ----------------------------------------------------------------------------
   function Get_Register_Address (
     Port     : Serial_Port;
     Register : Serial_Port_Register_Type
   ) return System.Address
   with Pure_Function,
     Inline;

   ----------------------------------------------------------------------------
   --  Is_Tx_Empty
   --
   --  Purpose:
   --    This function tests whether a particular port's transmission buffer is
   --    ready to accept new data.
   --    This is used during the various transmission functions to ensure that
   --    an overflow exception is not generated.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Is_Tx_Empty (
     Port : Serial_Port
   ) return Boolean
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Put_Char
   --
   --  Purpose:
   --    This procedure prints a character to a serial port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Put_Char (
     Port : Serial_Port;
     Data : Unsigned_8
   );

   ----------------------------------------------------------------------------
   --  Set_Divisor_Latch_State
   --
   --  Purpose:
   --    This procedure sets the divisor latch state for a particular serial
   --    peripheral.
   --    This will set the DLAB state for the selected serial peripheral.
   --    For more information regarding the use of this procedure refer to the
   --    16550 UART documentation.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Set_Divisor_Latch_State (
     Port  : Serial_Port;
     State : Boolean
   );

   ----------------------------------------------------------------------------
   --  Set_Baud_Rate
   --
   --  Purpose:
   --    This procedure sets the baud rate for a particular serial port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Set_Baud_Rate (
     Port : Serial_Port;
     Rate : Baud_Rate
   );

   ----------------------------------------------------------------------------
   --  Set_Interrupt_Generation
   --
   --  Purpose:
   --    This procedure enables or disables the generation of interrupts
   --    of a particular type.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Generation (
     Port           : Serial_Port;
     Interrupt_Type : Serial_Interrupt_Type;
     Status         : Boolean
   );

end Cxos.Devices.Serial;
