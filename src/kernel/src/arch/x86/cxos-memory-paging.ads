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
with x86.Memory.Paging;

-------------------------------------------------------------------------------
--  CXOS.MEMORY.PAGING
--
--  Purpose:
--    This package contains code and defintions for working with paging.
-------------------------------------------------------------------------------
package Cxos.Memory.Paging is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  The index into the page directory used for recursively mapping
   --  a page directory to itself.
   ----------------------------------------------------------------------------
   PAGE_DIR_RECURSIVE_IDX : constant Natural := 1023;

   ----------------------------------------------------------------------------
   --  Recursive Page Directory Address
   --  The address at which the currently loaded page dir is recursively
   --  mapped to itself.
   ----------------------------------------------------------------------------
   PAGE_DIR_RECURSIVE_MAP_ADDR : constant Integer_Address := 16#FFFF_F000#;

   ----------------------------------------------------------------------------
   --  Recursive Page Tables Address
   --  The address at which the currently loaded page tables are recursively
   --  mapped into the current address space.
   ----------------------------------------------------------------------------
   PAGE_TABLE_RECURSIVE_MAP_ADDR : constant Integer_Address := 16#FFFC_0000#;

   ----------------------------------------------------------------------------
   --  The index into the page directory used for mapping another page
   --  directory into the current address space.
   ----------------------------------------------------------------------------
   ALT_PAGE_DIR_IDX : constant Natural := 1022;

   ----------------------------------------------------------------------------
   --  Recursive Alt Page Directory Address
   --  The address at which a secondary page dir can be mapped into the
   --  current address space.
   ----------------------------------------------------------------------------
   ALT_PAGE_DIR_MAP_ADDR : constant Integer_Address := 16#FFFF_E000#;

   ----------------------------------------------------------------------------
   --  Recursive Alt Page Tables Address
   --  The address at which a secondary address space's tables can be  mapped
   --  into the current address space.
   ----------------------------------------------------------------------------
   ALT_PAGE_TABLE_MAP_ADDR : constant Integer_Address := 16#FFF8_0000#;

   ----------------------------------------------------------------------------
   --  Map_Page_Frame
   --
   --  Purpose:
   --    This function maps an individual page frame within the current
   --    virtual address space.
   --    This maps an individual 4K aligned virtual address frame to a
   --    physical address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Map_Page_Frame (
     Physical_Addr : System.Address;
     Virtual_Addr  : System.Address
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Enable_Paging
   --
   --  Purpose:
   --    This procedure enables paging on the processor and loads the initial
   --    kernel page directory address into the processor's control registers.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Load_Page_Directory (
      Directory_Ptr : System.Address
   )
   with Import,
     Convention    => Assembler,
     External_Name => "__load_page_directory";

   ----------------------------------------------------------------------------
   --  Allocate_Page_Frame
   --
   --  Purpose:
   --    This procedure allocates a page frame.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Allocate_Page_Frame (
     Virtual_Address :     System.Address;
     Frame_Address   : out x86.Memory.Paging.Page_Aligned_Address
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Insert_Page_Table
   --
   --  Purpose:
   --    This function inserts a new page table into the currently loaded page
   --    directory at a specified index.
   --    This will allocate a new page frame.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Insert_Page_Table (
     Directory_Idx : Natural;
     Supervisor    : Boolean := True
   ) return Process_Result
   with Volatile_Function;

end Cxos.Memory.Paging;
