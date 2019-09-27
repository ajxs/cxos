with System.Storage_Elements;

package body x86.IDT is
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      null;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Install_Descriptor
   ----------------------------------------------------------------------------
   procedure Install_Descriptor (
     Index       : in Descriptor_Entry_Range;
     Offset_Addr : in System.Address;
     Selector    : in Descriptor_Entry_Range;
     Privilege   : in Descriptor_Privilege_Level := Ring_0
   ) is
   begin
      --  Set the descriptor's offset fields.
      --  If an overflow occurs here the procedure will exit.
      Set_Descriptor_Offset :
         declare
            Offset : constant Unsigned_32 :=
              Unsigned_32 (To_Integer (Offset_Addr));
         begin
            Interrupt_Descriptor_Table (Index).Offset_Low :=
              Unsigned_16 (Offset and 16#FFFF#);
            Interrupt_Descriptor_Table (Index).Offset_High :=
              Unsigned_16 (Shift_Right (Offset, 16) and 16#FFFF#);
         exception
            when Constraint_Error =>
               return;
         end Set_Descriptor_Offset;

      Interrupt_Descriptor_Table (Index).Selector := Unsigned_16 (Selector);
      Interrupt_Descriptor_Table (Index).DPL      := Privilege;

   exception
      when Constraint_Error =>
         return;
   end Install_Descriptor;
end x86.IDT;
