with Interfaces;

-------------------------------------------------------------------------------
--  SYSTEM.X86.SERIAL
--
--  Purpose:
--    This package contains a basic Serial I/O driver.
--    The procedures and type definitions contained within this module can be
--    used to interact with the system's serial ports.
-------------------------------------------------------------------------------
package System.x86.Serial is
   pragma Preelaborate (System.x86.Serial);

   use Interfaces;

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
   with Pure_Function;

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

end System.x86.Serial;
