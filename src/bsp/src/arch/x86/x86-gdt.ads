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

with Interfaces; use Interfaces;
with x86.Descriptors; use x86.Descriptors;

-------------------------------------------------------------------------------
--  X86.GDT
--
--  Purpose:
--    This package contains definitions forthe Global Descriptor Table.
-------------------------------------------------------------------------------
package x86.GDT is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  The type of this memory segment.
   ----------------------------------------------------------------------------
   type Segment_Type is (
     Code,
     Data,
     None,
     Task_Type
   );

   ----------------------------------------------------------------------------
   --  Descriptor type information.
   --  Refer to Page 100. Intel IA-32 SDM 3a.
   ----------------------------------------------------------------------------
   type Descriptor_Type_T is
      record
         A          : Boolean;
         W_R        : Boolean;
         E_C        : Boolean;
         Field_Type : Boolean;
      end record
   with Size => 4;
   for Descriptor_Type_T use
      record
         A          at 0 range 0 .. 0;
         W_R        at 0 range 1 .. 1;
         E_C        at 0 range 2 .. 2;
         Field_Type at 0 range 3 .. 3;
      end record;

   ----------------------------------------------------------------------------
   --  An individual segment descriptor within the GDT.
   --  Refer to Page 98. Intel IA-32 SDM 3a.
   ----------------------------------------------------------------------------
   type GDT_Descriptor is
      record
         Limit_Low       : Unsigned_16;
         Base_Low        : Unsigned_16;
         Base_Mid        : Unsigned_8;
         Descriptor_Type : Descriptor_Type_T;
         S               : Boolean;
         DPL             : Descriptor_Privilege_Level;
         P               : Boolean;
         Limit_High      : Unsigned_4;
         AVL             : Boolean;
         L               : Boolean;
         DB              : Boolean;
         G               : Boolean;
         Base_High       : Unsigned_8;
      end record
   with Size => 64;
   for GDT_Descriptor use
      record
         Limit_Low       at 0 range 0  .. 15;
         Base_Low        at 0 range 16 .. 31;
         Base_Mid        at 4 range 0  .. 7;
         Descriptor_Type at 4 range 8  .. 11;
         S               at 4 range 12 .. 12;
         DPL             at 4 range 13 .. 14;
         P               at 4 range 15 .. 15;
         Limit_High      at 4 range 16 .. 19;
         AVL             at 4 range 20 .. 20;
         L               at 4 range 21 .. 21;
         DB              at 4 range 22 .. 22;
         G               at 4 range 23 .. 23;
         Base_High       at 4 range 24 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  GDT table type.
   ----------------------------------------------------------------------------
   type Global_Descriptor_Table_T is
     array (Descriptor_Entry_Range range <>) of GDT_Descriptor;

end x86.GDT;
