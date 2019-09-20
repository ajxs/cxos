package body x86.GDT is
   ----------------------------------------------------------------------------
   --  Finalise
   --
   --  Purpose:
   --    This procedure finalises the initialisation of the GDT.
   --    This function initiates the loading of the global descriptor table and
   --    the final jump to protected mode.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Finalise is
   begin
      --  Flush the GDT and jump to protected mode.
      Flush_Gdt;
   end Finalise;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the x86 platform's Global Descriptor Table.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      --  Install the individual segment descriptors.
      Install_Descriptor (0);
      Install_Descriptor (1, 0, 16#FFFFFFFF#, Ring_0, Code);
      Install_Descriptor (2, 0, 16#FFFFFFFF#, Ring_0, Data);
      Install_Descriptor (3, 0, 16#FFFFFFFF#, Ring_3, Code);
      Install_Descriptor (4, 0, 16#FFFFFFFF#, Ring_3, Data);

      GDT_Ptr.Size   := Global_Descriptor_Table'Size - 1;
      GDT_Ptr.Offset := Global_Descriptor_Table'Address;
   exception
      when Constraint_Error =>
         null;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure creates an individual descriptor entry in the x86
   --    platform's Global Descriptor Table.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Install_Descriptor (
     Index      : in Descriptor_Entry_Range;
     Base       : in Unsigned_32 := 0;
     Limit      : in Unsigned_32 := 0;
     Privilege  : in Privilege_Level := Ring_0;
     Entry_Type : in Segment_Type := None
   ) is
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

      case Entry_Type is
         when None =>
            Global_Descriptor_Table (Index).P          := False;
            Global_Descriptor_Table (Index).DPL        := Ring_0;
            Global_Descriptor_Table (Index).G          := False;
            Global_Descriptor_Table (Index).Descr_Type := 0;
         when Code =>
            Global_Descriptor_Table (Index).S          := True;
            Global_Descriptor_Table (Index).P          := True;
            Global_Descriptor_Table (Index).DPL        := Privilege;
            Global_Descriptor_Table (Index).Descr_Type := 10;

            Global_Descriptor_Table (Index).AVL        := True;
            Global_Descriptor_Table (Index).L          := True;
            Global_Descriptor_Table (Index).DB         := True;
            Global_Descriptor_Table (Index).G          := False;
         when Data =>
            Global_Descriptor_Table (Index).S          := True;
            Global_Descriptor_Table (Index).P          := True;
            Global_Descriptor_Table (Index).DPL        := Privilege;
            Global_Descriptor_Table (Index).Descr_Type := 2;

            Global_Descriptor_Table (Index).AVL        := True;
            Global_Descriptor_Table (Index).L          := True;
            Global_Descriptor_Table (Index).DB         := True;
            Global_Descriptor_Table (Index).G          := False;
      end case;

   exception
      when Constraint_Error =>
         null;
   end Install_Descriptor;
end x86.GDT;
