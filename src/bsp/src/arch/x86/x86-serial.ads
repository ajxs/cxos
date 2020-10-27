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
--  X86.SERIAL
--
--  Purpose:
--    This package contains a basic Serial I/O driver.
--    The procedures and type definitions contained within this module can be
--    used to interact with the system's serial ports.
-------------------------------------------------------------------------------
package x86.Serial is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  The maximum supported baud rate.
   ----------------------------------------------------------------------------
   MAXIMUM_BAUD_RATE : constant := 115200;

   ----------------------------------------------------------------------------
   --  Serial Baud Rate
   ----------------------------------------------------------------------------
   subtype Baud_Rate is Natural range 50 .. MAXIMUM_BAUD_RATE;

   ----------------------------------------------------------------------------
   --  Serial Port Type
   --  Defines the serial ports that can be used in the system.
   ----------------------------------------------------------------------------
   type Serial_Port is (
     COM1,
     COM2,
     COM3,
     COM4
   );

   ----------------------------------------------------------------------------
   --  Serial Interrupt Type
   --  Defines the types of interrupts the serial port can generate.
   ----------------------------------------------------------------------------
   type Serial_Interrupt_Type is (
     Modem_Line_Status,
     Rx_Data_Available,
     Rx_Line_Status,
     Tx_Empty
   );

   ----------------------------------------------------------------------------
   --  Port Interrupt status/enable register type.
   --  This type can be used for getting/setting the interrupt generation
   --  status of a particular interrupt type.
   --  For more information refer to page 17 of the PC16550D datasheet.
   ----------------------------------------------------------------------------
   type Port_Interrupt_Status is
      record
         ERBFI : Boolean;
         ETBEI : Boolean;
         ELSI  : Boolean;
         EDSSI : Boolean;
      end record
   with Size => 8,
     Convention => C,
     Volatile;
   for Port_Interrupt_Status use
      record
         ERBFI at 0 range 0 .. 0;
         ETBEI at 0 range 1 .. 1;
         ELSI  at 0 range 2 .. 2;
         EDSSI at 0 range 3 .. 3;
      end record;

   ----------------------------------------------------------------------------
   --  Serial Port register type.
   ----------------------------------------------------------------------------
   type Serial_Port_Register_Type is (
     Rx_Buffer_Tx_Holding,
     Interrupt_Enable,
     Interrupt_Ident_FIFO_Control,
     Line_Control,
     Modem_Control,
     Line_Status,
     Modem_Status,
     Scratch
   );

   ----------------------------------------------------------------------------
   --  Byte_To_Port_Interrupt_Status
   --
   --  Purpose:
   --    Unchecked conversion to read a port's interrupt status from
   --    an IO port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Byte_To_Port_Interrupt_Status is
      new Ada.Unchecked_Conversion (
        Source => Unsigned_8,
        Target => Port_Interrupt_Status
      );

   ----------------------------------------------------------------------------
   --  Port_Interrupt_Status_To_Byte
   --
   --  Purpose:
   --    Unchecked conversion to write a port's interrupt status to
   --    an IO port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Port_Interrupt_Status_To_Byte is
      new Ada.Unchecked_Conversion (
        Source => Port_Interrupt_Status,
        Target => Unsigned_8
      );

end x86.Serial;
