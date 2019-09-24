-------------------------------------------------------------------------------
--  X86.Interrupts
--
--  Purpose:
--    This package contains code for managing x86 processor interrupts.
-------------------------------------------------------------------------------
package x86.Interrupts is
   pragma Preelaborate (x86.Interrupts);

   ----------------------------------------------------------------------------
   --  Clear_Interrupt_Flag
   --
   --  Purpose:
   --    This procedure clears the system interrupt flag.
   --    Clearing the interrupt flag causes the processor to ignore maskable
   --    external interrupts.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Clear_Interrupt_Flag;

   ----------------------------------------------------------------------------
   --  Set_Interrupt_Flag
   --
   --  Purpose:
   --    Sets the interrupt flag in the processor register. After the interrupt
   --    flag is set, the processor begins responding to external,
   --    maskable interrupts
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Flag;
end x86.Interrupts;
