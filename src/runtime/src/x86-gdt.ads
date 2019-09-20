with Interfaces;
with System;

-------------------------------------------------------------------------------
--  X86.GDT
--
--  Purpose:
--    This package contains code for initialising the Global Descriptor Table.
--    The initialisation procedure within is called by the system init code.
-------------------------------------------------------------------------------
package x86.GDT is
   pragma Preelaborate (x86.GDT);

   use Interfaces;

   procedure Finalise;

   procedure Initialise;
private

   subtype Descriptor_Entry_Range is Natural;

   ----------------------------------------------------------------------------
   --  The type of this memory segment.
   ----------------------------------------------------------------------------
   type Segment_Type is (
     Code,
     Data,
     None
   );

   ----------------------------------------------------------------------------
   --  The privilege level for this particular segment.
   --  This is the 'protection ring' that this segment is accessible from.
   ----------------------------------------------------------------------------
   type Privilege_Level is (
     Ring_0,
     Ring_1,
     Ring_2,
     Ring_3
   )
   with Size => 2;
   for Privilege_Level use (
     Ring_0 => 0,
     Ring_1 => 1,
     Ring_2 => 2,
     Ring_3 => 3
   );

   procedure Install_Descriptor (
     Index      : in Descriptor_Entry_Range;
     Base       : in Unsigned_32 := 0;
     Limit      : in Unsigned_32 := 0;
     Privilege  : in Privilege_Level := Ring_0;
     Entry_Type : in Segment_Type := None
   );

   --  Page 98. Intel IA-32 SDM 3a.
   type GDT_Descriptor is
      record
         Limit_Low   : Unsigned_16;

         Base_Low    : Unsigned_16;

         Base_Mid    : Unsigned_8;
         Descr_Type  : Unsigned_4;
         S           : Boolean;
         DPL         : Privilege_Level;
         P           : Boolean;

         Limit_High  : Unsigned_4;
         AVL         : Boolean;
         L           : Boolean;
         DB          : Boolean;
         G           : Boolean;
         Base_High   : Unsigned_8;
      end record
   with Size => 64;
   for GDT_Descriptor use
      record
         Limit_Low   at 0 range  0  .. 15;
         Base_Low    at 0 range  16 .. 31;

         Base_Mid    at 4 range  0  .. 7;
         Descr_Type  at 4 range  8  .. 11;
         S           at 4 range  12 .. 12;
         DPL         at 4 range  13 .. 14;
         P           at 4 range  15 .. 15;

         Limit_High  at 4 range  16 .. 19;
         AVL         at 4 range  20 .. 20;
         L           at 4 range  21 .. 21;
         DB          at 4 range  22 .. 22;
         G           at 4 range  23 .. 23;
         Base_High   at 4 range  24 .. 31;
      end record;

   type GDT_Table is array (Natural range <>) of GDT_Descriptor;

   GDT_LENGTH : constant := 5;

   Global_Descriptor_Table : GDT_Table (0 .. (GDT_LENGTH - 1))
   with Export,
     Convention    => C,
     External_Name => "global_descriptor_table",
     Volatile;

   type GDT_Pointer is
      record
         Size   : Unsigned_16;
         Offset : System.Address;
      end record
   with Size => 48;
   for GDT_Pointer use
      record
         Size   at 0 range 0  .. 15;
         Offset at 0 range 16 .. 47;
      end record;

   GDT_Ptr : GDT_Pointer
   with Export,
     Convention    => C,
     External_Name => "gdt_pointer",
     Volatile;

   procedure Flush_Gdt
   with Import,
     Convention    => C,
     External_Name => "_gdt_flush";

end x86.GDT;
