with Interfaces;
with System.Storage_Elements;

-------------------------------------------------------------------------------
--  X86.SERIAL
--
--  Purpose:
--    This package contains a basic Serial I/O driver.
--    The procedures and type definitions contained within this module can be
--    used to interact with the system's serial ports.
-------------------------------------------------------------------------------
package x86.Serial is
   use Interfaces;
   use System.Storage_Elements;

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
   function Is_Tx_Empty (
     Port : in Serial_Port
   ) return Boolean;

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

   COM1_Address : constant System.Address := To_Address (16#3F8#);
   COM2_Address : constant System.Address := To_Address (16#3F8#);
   COM3_Address : constant System.Address := To_Address (16#3F8#);
   COM4_Address : constant System.Address := To_Address (16#3F8#);
end x86.Serial;
