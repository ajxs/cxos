with Interfaces;
with System.Storage_Elements;
with System.x86.Port_IO;

package body System.x86.PIC is
   use Interfaces;
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Get_Port_Address
   --
   --  Implementation Notes:
   --   - Returns a constant value stored within the function.
   ----------------------------------------------------------------------------
   function Get_Controller_Base_Address (
      Controller : PIC_Controller
   ) return System.Address is
      PIC1_Base_Addr : constant System.Address := To_Address (16#20#);
      PIC2_Base_Addr : constant System.Address := To_Address (16#A0#);
   begin
      case Controller is
         when PIC1 =>
            return PIC1_Base_Addr;
         when PIC2 =>
            return PIC2_Base_Addr;
      end case;
   exception
      when Constraint_Error =>
         return Null_Address;
   end Get_Controller_Base_Address;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Implementation Notes:
   --   - Does not preserve configured interrupt mask during initialisation.
   --   - All interrupts are masked initially.
   ----------------------------------------------------------------------------
   procedure Initialise is
      PIC1_Addr : constant System.Address :=
        Get_Controller_Base_Address (PIC1);
      PIC2_Addr : constant System.Address :=
        Get_Controller_Base_Address (PIC2);
   begin
      --  Begin initialization mode.
      --  Send ICW1 to each PIC.
      System.x86.Port_IO.Outb (PIC1_Addr, 16#11#);
      System.x86.Port_IO.Outb (PIC2_Addr, 16#11#);

      --  Send ICW2 to 'remap' the PIC.
      --  This tells the PIC that IRQ0 should start at vector 32 into the
      --  IDT. This is because in protected mode the first 31 IDT vectors
      --  are reserved for processor exceptions. So we remap the PIC to
      --  use vectors 32+ for interrupts.
      System.x86.Port_IO.Outb (PIC1_Addr + Storage_Offset (1), 16#20#);
      --  PIC1 has 8 interrupt lines, so we 'remap' PIC2 to respond to
      --  interrupts from 16#28# onwards.
      System.x86.Port_IO.Outb (PIC2_Addr + Storage_Offset (1), 16#28#);

      --  ICW3 instructs PIC1 that it is the master PIC, and to use IRQ2
      --  to control PIC2 in slave mode.
      --  It is a hardware convention used by manufacturers to use IRQ2 as
      --  the slave cascade line.
      System.x86.Port_IO.Outb (PIC1_Addr + Storage_Offset (1), 16#04#);
      --  Tell the slave PIC its cascade identity.
      System.x86.Port_IO.Outb (PIC2_Addr + Storage_Offset (1), 16#02#);

      --  Instruct PIC1 and PIC2 that they are to be used in x86 mode.
      System.x86.Port_IO.Outb (PIC1_Addr + Storage_Offset (1), 16#01#);
      System.x86.Port_IO.Outb (PIC2_Addr + Storage_Offset (1), 16#01#);

      --  Mask all interrupts.
      System.x86.Port_IO.Outb (
        PIC1_Addr + Storage_Offset (1), not 16#0#);
      System.x86.Port_IO.Outb (
        PIC2_Addr + Storage_Offset (1), not 16#0#);

   end Initialise;

   ----------------------------------------------------------------------------
   --  Send_EOI
   ----------------------------------------------------------------------------
   procedure Send_EOI (
     IRQ : Interrupt_ID
   ) is
      --  Represents a default non-specific EOI signal.
      EOI_Signal : constant Unsigned_8 := 16#20#;
      PIC1_Addr  : constant System.Address :=
        Get_Controller_Base_Address (PIC1);
      PIC2_Addr  : constant System.Address :=
        Get_Controller_Base_Address (PIC2);
   begin
      --  Raise an exception if the IRQ line is above what the PIC is
      --  set to handle.
      if IRQ > 15 then
         raise Constraint_Error;
      end if;

      --  Send the signal.
      if IRQ >= 8 then
         System.x86.Port_IO.Outb  (PIC2_Addr, EOI_Signal);
      end if;

      --  Even if the IRQ line in question was on PIC2, we send an EOI
      --  signal to PIC1 since the cascade line was raised.
      System.x86.Port_IO.Outb  (PIC1_Addr, EOI_Signal);

   exception
      when Constraint_Error =>
         return;
   end Send_EOI;

   ----------------------------------------------------------------------------
   --  Set_Interrupt_Mask
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Mask (
     IRQ    : Interrupt_ID;
     Status : Boolean
   ) is
      Interrupt_Mask : Unsigned_8;

      PIC1_Addr : constant System.Address :=
        Get_Controller_Base_Address (PIC1);
      PIC2_Addr : constant System.Address :=
        Get_Controller_Base_Address (PIC2);
   begin
      --  Raise an exception if the IRQ line is above what the PIC is
      --  set to handle.
      if IRQ > 15 then
         raise Constraint_Error;
      end if;

      --  Get the existing mask values.
      Get_Existing_Mask :
         begin
            if IRQ >= 8 then
               Interrupt_Mask := System.x86.Port_IO.Inb (
                 PIC2_Addr + Storage_Offset (1));
            else
               Interrupt_Mask := System.x86.Port_IO.Inb (
                 PIC1_Addr + Storage_Offset (1));
            end if;
         end Get_Existing_Mask;

      --  Get the correct mask bit.
      Set_Mask_Bit :
         declare
            --  This is the amount we need to shift the mask bit to set
            --  the mask for a particular IRQ line.
            Shift_Amount : constant Integer := Integer (IRQ) mod 8;
         begin
            --  Set the interrupt mask value.
            if Status then
               Interrupt_Mask :=
                 Interrupt_Mask or Shift_Left (1, Shift_Amount);
            else
               Interrupt_Mask :=
                 Interrupt_Mask and (not Shift_Left (1, Shift_Amount));
            end if;
         end Set_Mask_Bit;

      --  Write the interrupt mask.
      if IRQ >= 8 then
         System.x86.Port_IO.Outb (PIC2_Addr + Storage_Offset (1),
           Interrupt_Mask);
      else
         System.x86.Port_IO.Outb (PIC1_Addr + Storage_Offset (1),
           Interrupt_Mask);
      end if;

   exception
      when Constraint_Error =>
         return;
   end Set_Interrupt_Mask;

end System.x86.PIC;
