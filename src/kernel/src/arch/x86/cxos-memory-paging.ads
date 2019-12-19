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

-------------------------------------------------------------------------------
--  CXOS.MEMORY.PAGING
--
--  Purpose:
--    This package contains code and defintions for working with paging.
-------------------------------------------------------------------------------
package Cxos.Memory.Paging is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Create_New_Page_Directory
   --
   --  Purpose:
   --    Allocates and initialises a new page directory, placing the address
   --    of the newly allocated directory in the provided parameter.
   ----------------------------------------------------------------------------
   function Create_New_Page_Directory (
     Page_Directory_Addr : out System.Address
   ) return Process_Result;

private
   ----------------------------------------------------------------------------
   --  The address at which the page directory is recursively mapped into
   --  the current address space.
   ----------------------------------------------------------------------------
   PAGE_DIR_MAPPED_ADDR : constant Integer_Address := 16#FFFF_F000#;

   ----------------------------------------------------------------------------
   --  The address at which the currently loaded page tables are recursively
   --  mapped into the current address space.
   ----------------------------------------------------------------------------
   PAGE_TABLES_MAPPED_ADDR : constant Integer_Address := 16#FFC0_0000#;

   ----------------------------------------------------------------------------
   --  Find_Free_Kernel_Page
   --
   --  Purpose:
   --    This function finds the first free page table within Kernel space.
   --    It sets the output parameters to the table and frame indexes into
   --    the relevant paging structures.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Find_Free_Kernel_Page (
     Table_Index : out Natural;
     Page_Index  : out Natural
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Get_Page_Table_Mapped_Address
   --
   --  Purpose:
   --    This function returns the recursively mapped address of a page table.
   --    This mapped address can be used to edit the page table within memory.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Get_Page_Table_Mapped_Address (
     Table_Index :     Natural;
     Mapped_Addr : out System.Address
   ) return Process_Result
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Get_Page_Table_Mapped_Address
   --
   --  Purpose:
   --    This function returns the recursively mapped address of a page table.
   --    This mapped address can be used to edit the page table within memory.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Get_Page_Table_Mapped_Address (
     Virtual_Addr :     System.Address;
     Mapped_Addr  : out System.Address
   ) return Process_Result
   with Pure_Function;

end Cxos.Memory.Paging;
