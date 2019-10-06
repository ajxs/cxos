with Interfaces;
with System;

-------------------------------------------------------------------------------
--  SYSTEM.X86.PORT_IO
--
--  Purpose:
--    This package contains functionality for port-mapped I/O on the x86
--    platform.
--    Functions are included for inputting and outputting data to port-mapped
--    addresses, useful for interacting with system peripherals.
-------------------------------------------------------------------------------
package System.x86.Port_IO is
   pragma Preelaborate (System.x86.Port_IO);

   ----------------------------------------------------------------------------
   --  Inb
   --
   --  Purpose:
   --    This function reads a byte from a particular IO port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Inb (
     Port : System.Address
   ) return Interfaces.Unsigned_8
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Outb
   --
   --  Purpose:
   --    This function writes a byte to a particular IO port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Outb (
     Port : System.Address;
     Data : Interfaces.Unsigned_8
   );
end System.x86.Port_IO;
