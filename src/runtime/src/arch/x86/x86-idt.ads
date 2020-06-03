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
--  X86.IDT
--
--  Purpose:
--    This package contains definitions for the Interrupt Descriptor
--    Table.
-------------------------------------------------------------------------------
package x86.IDT is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Descriptor type information.
   --  Differs from GDT descriptor type field.
   --  Refer to Page 197. Intel IA-32 SDM 3a.
   ----------------------------------------------------------------------------
   type Descriptor_Type is (
     None,
     Interrupt_Gate_16_Bit,
     Interrupt_Gate_32_Bit
   )
   with Size => 4;
   for Descriptor_Type use (
     None                  => 0,
     Interrupt_Gate_16_Bit => 16#6#,
     Interrupt_Gate_32_Bit => 16#E#
   );

   ----------------------------------------------------------------------------
   --  An individual segment descriptor within the IDT.
   --  Refer to Page 197. Intel IA-32 SDM 3a.
   ----------------------------------------------------------------------------
   type IDT_Descriptor is
      record
         Offset_Low  : Unsigned_16;
         Selector    : Unsigned_16;
         Reserved    : Unsigned_8;
         Descr_Type  : Descriptor_Type;
         S           : Boolean;
         DPL         : Descriptor_Privilege_Level;
         P           : Boolean;
         Offset_High : Unsigned_16;
      end record
   with Size => 64;
   for IDT_Descriptor use
      record
         Offset_Low  at 0 range 0  .. 15;
         Selector    at 0 range 16 .. 31;
         Reserved    at 4 range 0  .. 7;
         Descr_Type  at 4 range 8  .. 11;
         S           at 4 range 12 .. 12;
         DPL         at 4 range 13 .. 14;
         P           at 4 range 15 .. 15;
         Offset_High at 4 range 16 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  IDT table type.
   ----------------------------------------------------------------------------
   type Interrupt_Descriptor_Table_T is
     array (Descriptor_Entry_Range range <>) of IDT_Descriptor;
end x86.IDT;
