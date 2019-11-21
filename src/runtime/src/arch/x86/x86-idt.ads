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
with System;
with x86.Descriptors;

-------------------------------------------------------------------------------
--  SYSTEM.X86.IDT
--
--  Purpose:
--    This package contains code for initialising the Interrupt Descriptor
--    Table. The initialisation procedure within is called by the system
--    init code.
-------------------------------------------------------------------------------
package x86.IDT is
   pragma Preelaborate (x86.IDT);

   use Interfaces;
   use x86.Descriptors;

   ----------------------------------------------------------------------------
   --  Finalise
   --
   --  Purpose:
   --    This procedure loads the IDT into the processor register.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Finalise
   with Import,
     Convention    => Assembler,
     External_Name => "__idt_load";

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the x86 platform's Interrupt
   --    Descriptor Table.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise;

   ----------------------------------------------------------------------------
   --  Install_Descriptor
   --
   --  Purpose:
   --    This procedure creates an individual descriptor entry in the x86
   --    platform's Interrupt Descriptor Table.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Install_Descriptor (
     Index       : Descriptor_Entry_Range;
     Offset_Addr : System.Address;
     Selector    : Unsigned_16;
     Privilege   : Descriptor_Privilege_Level := Ring_0
   );

private
   ----------------------------------------------------------------------------
   --  Initialise_Descriptor
   --
   --  Purpose:
   --    This initialises a descriptor entry. It creates an unused descriptor
   --    entry at an arbitrary position in the IDT.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise_Descriptor (
     Index : Descriptor_Entry_Range
   );

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

   type IDT_Table is array (Descriptor_Entry_Range range <>) of IDT_Descriptor;

   ----------------------------------------------------------------------------
   --  The number of entries in the Global Descriptor Table.
   --  Room for this number of entries is statically allocated.
   ----------------------------------------------------------------------------
   IDT_LENGTH : constant := 256;

   ----------------------------------------------------------------------------
   --  The actual Interrupt descriptor table entity.
   --  The length of the entries is statically allocated.
   ----------------------------------------------------------------------------
   Interrupt_Descriptor_Table : IDT_Table (0 .. (IDT_LENGTH - 1))
   with Alignment  => 8,
     Export,
     Convention    => Assembler,
     External_Name => "interrupt_descriptor_table",
     Volatile;

   ----------------------------------------------------------------------------
   --  The pointer to the IDT needed by the processor to load the IDT.
   ----------------------------------------------------------------------------
   IDT_Ptr : System_Table_Descriptor
   with Export,
     Convention    => Assembler,
     External_Name => "idt_pointer",
     Volatile;
end x86.IDT;
