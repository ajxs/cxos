package body x86.GDT is
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
      --  Initialise the descriptor entry. The segment base and limit
      --  addresses are computed and set here.
      --  The Descriptor is set to a code entry by default.
      --  If an overflow occurs here the procedure will exit.
      Initialise_Descriptor :
         declare
            --  These constants are used to compute the split
            --  base and offset entry values.
            Base  : constant Unsigned_32 :=
              Unsigned_32 (To_Integer (Base_Addr));
            Limit : constant Unsigned_32 :=
              Unsigned_32 (To_Integer (Limit_Addr));
         begin
            Global_Descriptor_Table (Index) := (
               Limit_Low  => Unsigned_16 (Limit and 16#FFFF#),
               Base_Low   => Unsigned_16 (Base and 16#FFFF#),
               Base_Mid   => Unsigned_8 (Shift_Right (Base, 16) and 16#FF#),
               Descr_Type => (
                  A          => False,
                  W_R        => True,
                  E_C        => False,
                  Field_Type => True
               ),
               S          => True,
               DPL        => Privilege,
               P          => True,
               Limit_High => Unsigned_4 (Shift_Right (Limit, 16) and 16#F#),
               AVL        => False,
               L          => False,
               DB         => True,
               G          => True,
               Base_High  => Unsigned_8 (Shift_Right (Base, 24) and 16#FF#)
            );
         exception
            when Constraint_Error =>
               return;
         end Initialise_Descriptor;

      case Entry_Type is
         when Code =>
            --  The descriptor is already configured as a valid
            --  code segment.
            null;
         when Data =>
            Global_Descriptor_Table (Index).Descr_Type.Field_Type := False;
         when None =>
            Global_Descriptor_Table (Index).Descr_Type := (
               A          => False,
               W_R        => False,
               E_C        => False,
               Field_Type => False
            );
            Global_Descriptor_Table (Index).S   := False;
            Global_Descriptor_Table (Index).P   := False;
            Global_Descriptor_Table (Index).AVL := False;
            Global_Descriptor_Table (Index).L   := False;
            Global_Descriptor_Table (Index).DB  := False;
            Global_Descriptor_Table (Index).G   := False;
      end case;
   exception
      when Constraint_Error =>
         null;
   end Install_Descriptor;
end x86.GDT;
