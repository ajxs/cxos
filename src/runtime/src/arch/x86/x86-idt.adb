-------------------------------------------------------------------------------
--  Copyright (c) 2019, CXOS.
--  This program is free software; you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3 of the License, or
--  (at your option) any later version.
--
--  Authors:
--     Anthony <ajxs [at] panoptic.online>
-------------------------------------------------------------------------------

with System.Storage_Elements; use System.Storage_Elements;

package body x86.IDT is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      --  Initialise all IDT Entries.
      Initialise_Loop :
         for I in Descriptor_Entry_Range range 0 .. IDT_LENGTH loop
            Initialise_Descriptor (I);
         end loop Initialise_Loop;

      --  Initialise the IDT pointer.
      IDT_Ptr.Size   := Interrupt_Descriptor_Table'Size - 1;
      IDT_Ptr.Offset := Interrupt_Descriptor_Table'Address;
   exception
      when Constraint_Error =>
         null;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Initialise_Descriptor
   --
   --  Implementation Notes:
   --    - Zeroes out an individual descriptor entry.
   ----------------------------------------------------------------------------
   procedure Initialise_Descriptor (
     Index : Descriptor_Entry_Range
   ) is
   begin
      Interrupt_Descriptor_Table (Index) := (
         Offset_Low  => 0,
         Selector    => 0,
         Reserved    => 0,
         Descr_Type  => None,
         S           => False,
         DPL         => Ring_0,
         P           => False,
         Offset_High => 0
      );

   exception
      when Constraint_Error =>
         return;
   end Initialise_Descriptor;

   ----------------------------------------------------------------------------
   --  Install_Descriptor
   ----------------------------------------------------------------------------
   procedure Install_Descriptor (
     Index       : Descriptor_Entry_Range;
     Offset_Addr : System.Address;
     Selector    : Unsigned_16;
     Privilege   : Descriptor_Privilege_Level := Ring_0
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

      Interrupt_Descriptor_Table (Index).Selector   := Selector;
      Interrupt_Descriptor_Table (Index).Descr_Type := Interrupt_Gate_32_Bit;
      Interrupt_Descriptor_Table (Index).Reserved   := 0;
      Interrupt_Descriptor_Table (Index).S          := False;
      Interrupt_Descriptor_Table (Index).DPL        := Privilege;
      Interrupt_Descriptor_Table (Index).P          := True;

   exception
      when Constraint_Error =>
         return;
   end Install_Descriptor;
end x86.IDT;
