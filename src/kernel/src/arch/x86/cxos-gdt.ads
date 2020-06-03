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

with System;
with System.Storage_Elements; use System.Storage_Elements;
with x86.Descriptors; use x86.Descriptors;
with x86.GDT; use x86.GDT;

-------------------------------------------------------------------------------
--  CXOS.GDT
--
--  Purpose:
--    This package contains code for initialising the Global Descriptor Table.
--    The initialisation procedure within is called by the system init code.
-------------------------------------------------------------------------------
package Cxos.GDT is
   pragma Preelaborate;

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
     External_Name => "cxos_gdt_load";

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
   --  The number of entries in the Global Descriptor Table.
   --  Room for this number of entries is statically allocated.
   ----------------------------------------------------------------------------
   GDT_LENGTH : constant := 16;

   ----------------------------------------------------------------------------
   --  The actual global descriptor table entity.
   --  The length of the entries is statically allocated.
   ----------------------------------------------------------------------------
   Global_Descriptor_Table :
     Global_Descriptor_Table_T (0 .. (GDT_LENGTH - 1)) := (others =>
     (
       Limit_Low  => 0,
       Base_Low   => 0,
       Base_Mid   => 0,
       Descr_Type => (
         A          => False,
         W_R        => False,
         E_C        => False,
         Field_Type => False
       ),
       S          => False,
       DPL        => Ring_0,
       P          => False,
       Limit_High => 0,
       AVL        => False,
       L          => False,
       DB         => False,
       G          => False,
       Base_High  => 0
     ))
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

end Cxos.GDT;
