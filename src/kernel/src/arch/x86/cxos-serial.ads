-------------------------------------------------------------------------------
--  CXOS.SERIAL
--
--  Purpose:
--    This package contains functionality for interfacing with the target
--    architecture's serial hardware.
-------------------------------------------------------------------------------
package Cxos.Serial is
   pragma Preelaborate (Cxos.Serial);

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
end Cxos.Serial;
