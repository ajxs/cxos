with System.Machine_Code;

package body x86.Interrupts is
   ----------------------------------------------------------------------------
   --  Clear_Interrupt_Flag
   ----------------------------------------------------------------------------
   procedure Clear_Interrupt_Flag is
   begin
      System.Machine_Code.Asm (
        Template => "cli",
        Volatile => True);
   end Clear_Interrupt_Flag;

   ----------------------------------------------------------------------------
   --  Set_Interrupt_Flag
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Flag is
   begin
      System.Machine_Code.Asm (
        Template => "sti",
        Volatile => True);
   end Set_Interrupt_Flag;
end x86.Interrupts;
