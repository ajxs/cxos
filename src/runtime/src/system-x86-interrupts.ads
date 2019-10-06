-------------------------------------------------------------------------------
--  SYSTEM.X86.INTERRUPTS
--
--  Purpose:
--    This package contains code for managing x86 processor interrupts.
-------------------------------------------------------------------------------
package System.x86.Interrupts is
   pragma Preelaborate (System.x86.Interrupts);

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
end System.x86.Interrupts;
