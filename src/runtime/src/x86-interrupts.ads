-------------------------------------------------------------------------------
--  X86.Interrupts
--
--  Purpose:
--    This package contains code for managing x86 processor interrupts.
-------------------------------------------------------------------------------
package x86.Interrupts is
   pragma Preelaborate (x86.Interrupts);

   ----------------------------------------------------------------------------
   --  Set_Interrupt_Flag
   --
   --  Purpose:
   --    Sets or clears the interrupt flag in the processor EFLAGS register.
   --    If this is set to true, the processor begins responding to external
   --    maskable interrupts. Otherwise all maskable interrupts are ignored.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Flag (
     Status : Boolean
   );
end x86.Interrupts;
