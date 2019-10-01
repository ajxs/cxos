with x86.Interrupts;

package body x86.GDT is
   ----------------------------------------------------------------------------
   --  Finalise
   --
   --  Implementation Notes:
   --   - It is assumed by the flush GDT procedure that the bootloader has
   --     already placed the system in protected mode.
   ----------------------------------------------------------------------------
   procedure Finalise is
   begin
      --  Clear interrupts.
      x86.Interrupts.Set_Interrupt_Flag (False);

      --  Flush the GDT and reload.
      Flush_Gdt;

      --  Enable Interrupts.
      x86.Interrupts.Set_Interrupt_Flag (True);
   end Finalise;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
      Flat_Base  : constant System.Address := To_Address (0);
      Flat_Limit : constant System.Address := To_Address (16#FFFFF#);
   begin
      --  Install the individual segment descriptors.
      Install_Descriptor (0);
      Install_Descriptor (1, Flat_Base, Flat_Limit, Ring_0, Code);
      Install_Descriptor (2, Flat_Base, Flat_Limit, Ring_0, Data);
      Install_Descriptor (3, Flat_Base, Flat_Limit, Ring_3, Code);
      Install_Descriptor (4, Flat_Base, Flat_Limit, Ring_3, Data);

      GDT_Ptr.Size   := Global_Descriptor_Table'Size - 1;
      GDT_Ptr.Offset := Global_Descriptor_Table'Address;
   exception
      when Constraint_Error =>
         null;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Install_Descriptor
   --
   --  Implementation Notes:
   --   - Even though the limit param will accept a full 32-bit memory address
   --     only the lower 20 bits will be used.
   --   - 4kb granularity is always enabled.
   ----------------------------------------------------------------------------
   procedure Install_Descriptor (
     Index      : Descriptor_Entry_Range;
     Base_Addr  : System.Address             := To_Address (0);
     Limit_Addr : System.Address             := To_Address (0);
     Privilege  : Descriptor_Privilege_Level := Ring_0;
     Entry_Type : Segment_Type               := None
   ) is
   begin
      --  Set the segment base and limit addresses.
      --  If an overflow occurs here the procedure will exit.
      Set_Segment_Addresses :
         declare
            Base  : constant Unsigned_32 :=
              Unsigned_32 (To_Integer (Base_Addr));
            Limit : constant Unsigned_32 :=
              Unsigned_32 (To_Integer (Limit_Addr));
         begin
            Global_Descriptor_Table (Index).Limit_Low :=
              Unsigned_16 (Limit and 16#FFFF#);
            Global_Descriptor_Table (Index).Limit_High :=
              Unsigned_4 (Shift_Right (Limit, 16) and 16#F#);

            Global_Descriptor_Table (Index).Base_Low :=
              Unsigned_16 (Base and 16#FFFF#);
            Global_Descriptor_Table (Index).Base_Mid :=
              Unsigned_8 (Shift_Right (Base, 16) and 16#FF#);
            Global_Descriptor_Table (Index).Base_High :=
              Unsigned_8 (Shift_Right (Base, 24) and 16#FF#);
         exception
            when Constraint_Error =>
               return;
         end Set_Segment_Addresses;

      case Entry_Type is
         when Code =>
            Global_Descriptor_Table (Index).S          := True;
            Global_Descriptor_Table (Index).P          := True;
            Global_Descriptor_Table (Index).DPL        := Privilege;
            Global_Descriptor_Table (Index).Descr_Type := (
              A          => False,
              W_R        => True,
              E_C        => False,
              Field_Type => True
            );

            Global_Descriptor_Table (Index).AVL        := True;
            Global_Descriptor_Table (Index).L          := True;
            Global_Descriptor_Table (Index).DB         := True;
            Global_Descriptor_Table (Index).G          := True;
         when Data =>
            Global_Descriptor_Table (Index).S          := True;
            Global_Descriptor_Table (Index).P          := True;
            Global_Descriptor_Table (Index).DPL        := Privilege;
            Global_Descriptor_Table (Index).Descr_Type := (
              A          => False,
              W_R        => True,
              E_C        => False,
              Field_Type => False
            );

            Global_Descriptor_Table (Index).AVL        := True;
            Global_Descriptor_Table (Index).L          := True;
            Global_Descriptor_Table (Index).DB         := True;
            Global_Descriptor_Table (Index).G          := True;
         when None =>
            Global_Descriptor_Table (Index).P          := False;
            Global_Descriptor_Table (Index).DPL        := Ring_0;
            Global_Descriptor_Table (Index).G          := False;
            Global_Descriptor_Table (Index).Descr_Type := (
              A          => False,
              W_R        => False,
              E_C        => False,
              Field_Type => False
            );
      end case;
   exception
      when Constraint_Error =>
         null;
   end Install_Descriptor;
end x86.GDT;
