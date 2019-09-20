with Interfaces;
with System;

-------------------------------------------------------------------------------
--  X86.PORT_IO
--
--  Purpose:
--    This package contains functionality for port-mapped I/O on the x86
--    platform.
--    Functions are included for inputting and outputting data to port-mapped
--    addresses, useful for interacting with system peripherals.
-------------------------------------------------------------------------------
package x86.Port_IO is
   pragma Preelaborate (x86.Port_IO);

   function Inb (
     Port : System.Address
   ) return Interfaces.Unsigned_8
   with Volatile_Function;

   procedure Outb (
     Port : System.Address;
     Data : Interfaces.Unsigned_8
   );
end x86.Port_IO;
