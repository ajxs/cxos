with Interfaces;
with System;
with System.Storage_Elements;
with System.x86.Descriptors;

-------------------------------------------------------------------------------
--  SYSTEM.X86.GDT
--
--  Purpose:
--    This package contains code for initialising the Global Descriptor Table.
--    The initialisation procedure within is called by the system init code.
-------------------------------------------------------------------------------
package System.x86.GDT is
   pragma Preelaborate (System.x86.GDT);

   use Interfaces;
   use System.Storage_Elements;
   use System.x86.Descriptors;

   ----------------------------------------------------------------------------
   --  Finalise
   --
   --  Purpose:
   --    This procedure finalises the initialisation of the GDT.
   --    This function initiates the loading of the global descriptor table.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Finalise
   with Import,
     Convention    => Assembler,
     External_Name => "__gdt_load";

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the x86 platform's Global Descriptor Table.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise;

private
   ----------------------------------------------------------------------------
   --  The type of this memory segment.
   ----------------------------------------------------------------------------
   type Segment_Type is (
     Code,
     Data,
     None
   );

   ----------------------------------------------------------------------------
   --  Descriptor type information.
   --  Refer to Page 100. Intel IA-32 SDM 3a.
   ----------------------------------------------------------------------------
   type Descriptor_Type is
      record
         A          : Boolean;
         W_R        : Boolean;
         E_C        : Boolean;
         Field_Type : Boolean;
      end record
   with Size => 4;
   for Descriptor_Type use
      record
         A          at 0 range 0 .. 0;
         W_R        at 0 range 1 .. 1;
         E_C        at 0 range 2 .. 2;
         Field_Type at 0 range 3 .. 3;
      end record;

   ----------------------------------------------------------------------------
   --  Install_Descriptor
   --
   --  Purpose:
   --    This procedure creates an individual descriptor entry in the x86
   --    platform's Global Descriptor Table.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Install_Descriptor (
     Index      : Descriptor_Entry_Range;
     Base_Addr  : System.Address             := To_Address (0);
     Limit_Addr : System.Address             := To_Address (0);
     Privilege  : Descriptor_Privilege_Level := Ring_0;
     Entry_Type : Segment_Type               := None
   );

   ----------------------------------------------------------------------------
   --  An individual segment descriptor within the GDT.
   --  Refer to Page 98. Intel IA-32 SDM 3a.
   ----------------------------------------------------------------------------
   type GDT_Descriptor is
      record
         Limit_Low  : Unsigned_16;
         Base_Low   : Unsigned_16;
         Base_Mid   : Unsigned_8;
         Descr_Type : Descriptor_Type;
         S          : Boolean;
         DPL        : Descriptor_Privilege_Level;
         P          : Boolean;
         Limit_High : Unsigned_4;
         AVL        : Boolean;
         L          : Boolean;
         DB         : Boolean;
         G          : Boolean;
         Base_High  : Unsigned_8;
      end record
   with Size => 64;
   for GDT_Descriptor use
      record
         Limit_Low   at 0 range 0  .. 15;
         Base_Low    at 0 range 16 .. 31;
         Base_Mid    at 4 range 0  .. 7;
         Descr_Type  at 4 range 8  .. 11;
         S           at 4 range 12 .. 12;
         DPL         at 4 range 13 .. 14;
         P           at 4 range 15 .. 15;
         Limit_High  at 4 range 16 .. 19;
         AVL         at 4 range 20 .. 20;
         L           at 4 range 21 .. 21;
         DB          at 4 range 22 .. 22;
         G           at 4 range 23 .. 23;
         Base_High   at 4 range 24 .. 31;
      end record;

   type GDT_Table is array (Descriptor_Entry_Range range <>) of GDT_Descriptor;

   ----------------------------------------------------------------------------
   --  The number of entries in the Global Descriptor Table.
   --  Room for this number of entries is statically allocated.
   ----------------------------------------------------------------------------
   GDT_LENGTH : constant := 5;

   ----------------------------------------------------------------------------
   --  The actual global descriptor table entity.
   --  The length of the entries is statically allocated.
   ----------------------------------------------------------------------------
   Global_Descriptor_Table : GDT_Table (0 .. (GDT_LENGTH - 1))
   with Alignment  => 16,
     Export,
     Convention    => Assembler,
     External_Name => "global_descriptor_table",
     Volatile;

   ----------------------------------------------------------------------------
   --  The pointer to the GDT needed by the processor to load the GDT.
   ----------------------------------------------------------------------------
   GDT_Ptr : System_Table_Descriptor
   with Export,
     Convention    => Assembler,
     External_Name => "gdt_pointer",
     Volatile;

end System.x86.GDT;
