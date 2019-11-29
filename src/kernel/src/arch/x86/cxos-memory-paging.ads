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
