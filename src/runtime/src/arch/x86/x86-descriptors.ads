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

with Interfaces;

-------------------------------------------------------------------------------
--  SYSTEM.X86.DESCRIPTORS
--
--  Purpose:
--    This package contains definitions common to the use of descriptors in the
--    x86 system. These are used in the Global Descriptor Table (GDT) and the
--    Interrupt Descriptor Table (IDT).
--    Refer to 'system-x86-gdt.ads' and 'system-x86-idt.ads' respectively.
-------------------------------------------------------------------------------
package x86.Descriptors is
   pragma Preelaborate (x86.Descriptors);

   use Interfaces;

   ----------------------------------------------------------------------------
   --  The privilege level for a particular descriptor.
   --  These correspond to the 'protection ring' that this descriptor is
   --  accessible from.
   ----------------------------------------------------------------------------
   type Descriptor_Privilege_Level is (
     Ring_0,
     Ring_1,
     Ring_2,
     Ring_3
   )
   with Size => 2;
   for Descriptor_Privilege_Level use (
     Ring_0 => 0,
     Ring_1 => 1,
     Ring_2 => 2,
     Ring_3 => 3
   );

   ----------------------------------------------------------------------------
   --  Range type for Descriptor tables.
   ----------------------------------------------------------------------------
   subtype Descriptor_Entry_Range is Natural;

   ----------------------------------------------------------------------------
   --  The format of the System Table Descriptor pointer used by the processor
   --  to load descriptor tables like the GDT and IDT.
   ----------------------------------------------------------------------------
   type System_Table_Descriptor is
      record
         Size   : Unsigned_16;
         Offset : System.Address;
      end record
   with Size => 48;
   for System_Table_Descriptor use
      record
         Size   at 0 range 0  .. 15;
         Offset at 0 range 16 .. 47;
      end record;
end x86.Descriptors;
