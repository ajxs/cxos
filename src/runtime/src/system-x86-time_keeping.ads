pragma Restrictions (No_Elaboration_Code);

-------------------------------------------------------------------------------
--  SYSTEM.X86.TIME_KEEPING
--
--  Purpose:
--    This package contains code for working with the system time.
--    Functionality for time keeping is contained within this package. Such as
--    reading the current system clock as well as the functionality for
--    initialising and incrementing the timer.
-------------------------------------------------------------------------------
package System.x86.Time_Keeping is
   pragma Preelaborate (System.x86.Time_Keeping);

   ----------------------------------------------------------------------------
   --  Time type.
   --  Represents a millisecond count.
   ----------------------------------------------------------------------------
   type Time is mod 2 ** 64
   with Size => 64;

   ----------------------------------------------------------------------------
   --  Clock
   --
   --  Purpose:
   --    This function returns the current system time.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Clock return Time
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the system timer.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise;

   ----------------------------------------------------------------------------
   --  System_Tick_Handler
   --
   --  Purpose:
   --    This procedure handles a tick from the system timer.
   --    This is called by the IRQ0 handler, which is triggered by the
   --    system's Programmable Interval Timer.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure System_Tick_Handler;

private
   ----------------------------------------------------------------------------
   --  This is the period of time, measured in milliseconds, represented by a
   --  single processor timer 'tick'.
   ----------------------------------------------------------------------------
   SYSTEM_TICK_PERIOD : constant Time := 10;

   ----------------------------------------------------------------------------
   --  This is the system's internal time.
   ----------------------------------------------------------------------------
   System_Time : Time
   with Volatile;

end System.x86.Time_Keeping;
