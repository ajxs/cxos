with x86.GDT;
with x86.Serial;

package body x86 is
   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the x86 platform.
   --    This will perform all the necessary initialisation in order to load
   --    and begin execution of the kernel.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      x86.Serial.Initialise (x86.Serial.COM1, 38400);

      x86.Serial.Put_String (x86.Serial.COM1, "Initialising GDT" & ASCII.LF);
      x86.GDT.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1, "Flushing GDT" & ASCII.LF);
      x86.GDT.Finalise;

   end Initialise;

   ----------------------------------------------------------------------------
   --  Last_Chance_Handler
   --
   --  Purpose:
   --    The runtime Last_Chance_Handler function.
   --    This procedure is the GNAT mandated handler for any uncaught
   --    exceptions that are propagated to the top level.
   --    This runtime, like other bareboard targets, does not support exception
   --    propagation. So any uncaught exception will be handled here.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Last_Chance_Handler (
     Msg  : System.Address;
     Line : Integer
   ) is
   begin
      null;
   end Last_Chance_Handler;
end x86;
