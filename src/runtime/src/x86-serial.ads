with Interfaces;

-------------------------------------------------------------------------------
--  X86.SERIAL
--
--  Purpose:
--    This package contains a basic Serial I/O driver.
--    The procedures and type definitions contained within this module can be
--    used to interact with the system's serial ports.
-------------------------------------------------------------------------------
package x86.Serial is
   pragma Preelaborate (x86.Serial);

   use Interfaces;

   MAXIMUM_BAUD_RATE : constant := 115200;

   subtype Baud_Rate is Natural range 50 .. MAXIMUM_BAUD_RATE;

   type Serial_Port is (
     COM1,
     COM2,
     COM3,
     COM4
   );

   procedure Initialise (
     Port : in Serial_Port;
     Rate : in Baud_Rate := MAXIMUM_BAUD_RATE
   );

   procedure Put_String (
     Port : in Serial_Port;
     Data : in String
   );

private
   function Get_Port_Address (
     Port : in Serial_Port
   ) return System.Address
   with Pure_Function;

   function Is_Tx_Empty (
     Port : in Serial_Port
   ) return Boolean
   with Volatile_Function;

   procedure Put_Char (
     Port : in Serial_Port;
     Data : in Unsigned_8
   );

   procedure Set_Baud_Rate (
     Port : in Serial_Port;
     Rate : in Baud_Rate
   );

   procedure Set_Divisor_Latch_State (
     Port  : in Serial_Port;
     State : in Boolean
   );

end x86.Serial;
