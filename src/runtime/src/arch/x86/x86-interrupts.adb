with System.Machine_Code;

package body x86.Interrupts is
   ----------------------------------------------------------------------------
   --  Set_Interrupt_Flag
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Flag (
     Status : Boolean
   ) is
   begin
      case Status is
         when True =>
            System.Machine_Code.Asm (
              Template => "sti",
              Volatile => True);
         when False =>
            System.Machine_Code.Asm (
              Template => "cli",
              Volatile => True);
      end case;
   exception
      when Constraint_Error =>
         return;
   end Set_Interrupt_Flag;
end x86.Interrupts;
