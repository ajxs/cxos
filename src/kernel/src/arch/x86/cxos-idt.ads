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
with System;
with x86.Descriptors; use x86.Descriptors;
with x86.IDT; use x86.IDT;

-------------------------------------------------------------------------------
--  CXOS.IDT
--
--  Purpose:
--    This package contains code for initialising the Interrupt Descriptor
--    Table. The initialisation procedure within is called by the system
--    init code.
-------------------------------------------------------------------------------
package Cxos.IDT is
   pragma Preelaborate;

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
     External_Name => "cxos_idt_load";

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
   --  The number of entries in the Global Descriptor Table.
   --  Room for this number of entries is statically allocated.
   ----------------------------------------------------------------------------
   IDT_LENGTH : constant := 256;

   ----------------------------------------------------------------------------
   --  The actual Interrupt descriptor table entity.
   --  The length of the entries is statically allocated.
   ----------------------------------------------------------------------------
   Interrupt_Descriptor_Table :
     Interrupt_Descriptor_Table_T (0 .. (IDT_LENGTH - 1))
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
end Cxos.IDT;
