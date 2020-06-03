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
   --  Create_New_Address_Space
   --
   --  Purpose:
   --    Allocates and initialises a new virtual address space, placing the
   --    address of the newly allocated directory in the provided parameter.
   --    This function will copy the kernel address space contents from the
   --    currently loaded page directory into the new address space.
   ----------------------------------------------------------------------------
   function Create_New_Address_Space (
     Page_Directory_Addr : out System.Address;
     Initial_EIP         :     System.Address
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Create_New_Kernel_Stack
   ----------------------------------------------------------------------------
   function Create_New_Kernel_Stack (
     Stack_Addr  : out System.Address;
     Initial_EIP :     System.Address
   ) return Process_Result;

   ----------------------------------------------------------------------------
   --  Map_Virtual_Address
   --
   --  Purpose:
   --    Maps a virtual address to a physical address in the specified page
   --    directory. This procedure does not acknowledge whether or not the
   --    provided virtual address is already mapped, and will overwrite any
   --    existing entry if present.
   --  Exceptions:
   --    Exception conditions will be returned if the provided addresses are
   --    not page-aligned.
   ----------------------------------------------------------------------------
   procedure Map_Virtual_Address (
     Page_Dir      :     x86.Memory.Paging.Page_Directory;
     Virtual_Addr  :     System.Address;
     Physical_Addr :     System.Address;
     Status        : out Process_Result;
     Read_Write    :     Boolean := True;
     User_Mode     :     Boolean := False
   );

   ----------------------------------------------------------------------------
   --  Temporarily_Map_Page
   --
   --  Purpose:
   --    This procedure temporarily maps a frame into the current address
   --    space. The output parameter is set to the temporarily mapped virtual
   --    address of the provided frame address.
   --  Exceptions:
   --    - An exception condition will be returned if there are no free
   --      frames in the temporary mapping table.
   ----------------------------------------------------------------------------
   procedure Temporarily_Map_Page (
     Frame_Addr   :     System.Address;
     Virtual_Addr : out System.Address;
     Status       : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Free_Temporary_Page_Mapping
   --
   --  Purpose:
   --    This procedure frees a temporarily mapped address from the temporary
   --    mapping table. It accepts an address corresponding to a mapping in
   --    the table.
   --  Exceptions:
   --    - An exception condition will be returned if the mapped address is
   --      not within the temporary mapping table.
   ----------------------------------------------------------------------------
   procedure Free_Temporary_Page_Mapping (
     Virtual_Addr :     System.Address;
     Status       : out Process_Result
   );

private
   ----------------------------------------------------------------------------
   --  The address at which the page directory is recursively mapped into
   --  the current address space.
   ----------------------------------------------------------------------------
   PAGE_DIR_RECURSIVE_ADDR : constant Integer_Address := 16#FFFF_F000#;

   ----------------------------------------------------------------------------
   --  The address at which the currently loaded page tables are recursively
   --  mapped into the current address space.
   ----------------------------------------------------------------------------
   PAGE_TABLES_BASE_ADDR : constant Integer_Address := 16#FFC0_0000#;

   ----------------------------------------------------------------------------
   --  The address at which the temporary mapping table is recursively
   --  mapped into the current address space.
   ----------------------------------------------------------------------------
   TEMP_TABLE_RECURSIVE_ADDR : constant Integer_Address := 16#FFFF_E000#;

   ----------------------------------------------------------------------------
   --  The base address at which the temporary mapping table contents are
   --  mapped into the current addres space.
   ----------------------------------------------------------------------------
   TEMP_TABLE_BASE_ADDR : constant Integer_Address := 16#FF80_0000#;

   ----------------------------------------------------------------------------
   --  Create_Page_Table
   --
   --  Purpose;
   --    Creates and initialises a new blank page table.
   ----------------------------------------------------------------------------
   function Create_Page_Table (
     Page_Table_Addr : out System.Address
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Find_Free_Kernel_Page
   --
   --  Purpose:
   --    This procedure finds the first free page table within Kernel space.
   --    It sets the output parameters to the table and frame indexes into
   --    the relevant paging structures.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Find_Free_Kernel_Page (
     Table_Index : out Natural;
     Page_Index  : out Natural;
     Status      : out Process_Result
   );

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

   ----------------------------------------------------------------------------
   --  Initialise_Page_Directory
   --
   --  Purpose:
   --    This initialises an individual Page Directory.
   --    It will initialise every entry in the directory as being non-present.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise_Page_Directory (
     Page_Dir : in out x86.Memory.Paging.Page_Directory;
     Status   :    out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Initialise_Page_Table
   --
   --  Purpose:
   --    This initialises an individual Page Table.
   --    The Table will be initialised with all entries marked as non-present.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise_Page_Table (
     Table  : in out x86.Memory.Paging.Page_Table;
     Status :    out Process_Result
   );

end Cxos.Memory.Paging;
