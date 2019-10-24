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

-------------------------------------------------------------------------------
--  SYSTEM.X86.MEMORY
--
--  Purpose:
--    This package contains code and defintions for implementing and working
--    with memory on the x86 platform.
-------------------------------------------------------------------------------
package x86.Memory is
   pragma Preelaborate (x86.Memory);

   ----------------------------------------------------------------------------
   --  Map_Kernel
   --
   --  Purpose:
   --    This procedure maps memory for the kernel. This is required to
   --    allocate memory for the kernel prior to enabling paging.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Map_Kernel;

   ----------------------------------------------------------------------------
   --  Enable_Paging
   --
   --  Purpose:
   --    This procedure finalises the loading of the page directory structures
   --    into the processor's control registers.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Enable_Paging
   with Import,
     Convention    => Assembler,
     External_Name => "__paging_load";

private
   ----------------------------------------------------------------------------
   --  Paging Process Result
   --  Used for storing and returning the result of an internal paging
   --  procedure.
   ----------------------------------------------------------------------------
   type Paging_Process_Result is (
     Frame_Allocation_Error,
     Frame_Not_Allocated,
     Invalid_Argument,
     Invalid_Non_Aligned_Address,
     Invalid_Table_Index,
     Success,
     Table_Not_Allocated
   );

   ----------------------------------------------------------------------------
   --  Type to hold a 20bit address.
   --  These are truncated 32bit addresses that are assumed to be 4K aligned,
   --  as such there is no need to hold the lower-order 12bits.
   ----------------------------------------------------------------------------
   type Page_Aligned_Address is mod 2 ** 20;

   ----------------------------------------------------------------------------
   --  Page Directory Entry type.
   ----------------------------------------------------------------------------
   type Page_Directory_Entry is
      record
         Present       : Boolean;
         Read_Write    : Boolean;
         U_S           : Boolean;
         PWT           : Boolean;
         PCD           : Boolean;
         A             : Boolean;
         Reserved      : Boolean := False;
         PS            : Boolean;
         G             : Boolean;
         Avail         : Boolean := False;
         Table_Address : Page_Aligned_Address;
      end record
   with Size => 32;
   for Page_Directory_Entry use
      record
         Present       at 0 range 0 .. 0;
         Read_Write    at 0 range 1 .. 1;
         U_S           at 0 range 2 .. 2;
         PWT           at 0 range 3 .. 3;
         PCD           at 0 range 4 .. 4;
         A             at 0 range 5 .. 5;
         Reserved      at 0 range 6 .. 6;
         PS            at 0 range 7 .. 7;
         G             at 0 range 8 .. 8;
         Avail         at 0 range 9 .. 11;
         Table_Address at 0 range 12 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  Page Frame type.
   ----------------------------------------------------------------------------
   type Page_Frame is
      record
         Present      : Boolean;
         Read_Write   : Boolean;
         U_S          : Boolean;
         PWT          : Boolean;
         PCD          : Boolean;
         A            : Boolean;
         D            : Boolean;
         PAT          : Boolean;
         G            : Boolean;
         Page_Address : Page_Aligned_Address;
      end record
   with Size => 32;
   for Page_Frame use
      record
         Present      at 0 range 0 .. 0;
         Read_Write   at 0 range 1 .. 1;
         U_S          at 0 range 2 .. 2;
         PWT          at 0 range 3 .. 3;
         PCD          at 0 range 4 .. 4;
         A            at 0 range 5 .. 5;
         D            at 0 range 6 .. 6;
         PAT          at 0 range 7 .. 7;
         G            at 0 range 8 .. 8;
         Page_Address at 0 range 12 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  Check_Address_Page_Aligned
   --
   --  Purpose:
   --    Checks whether a provided address is 4K aligned, as required by the
   --    paging entity structures.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Check_Address_Page_Aligned (
     Addr : System.Address
   ) return Boolean
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Convert_To_Page_Aligned_Address
   --
   --  Purpose:
   --    This function converts a System Address to the 20bit 4kb page aligned
   --    address type expected by the page table entities.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Convert_To_Page_Aligned_Address (
     Addr : System.Address
   ) return Page_Aligned_Address
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Convert_To_System_Address
   --
   --  Purpose:
   --    This function converts a 20bit 4kb aligned address type back to a
   --    native 32bit system address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Convert_To_System_Address (
     Addr : Page_Aligned_Address
   ) return System.Address
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Get_Page_Directory_Index
   --
   --  Purpose:
   --    This function gets the index of the page table in a page directory of
   --    corresponding to a particular address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Get_Page_Directory_Index (
     Addr  : System.Address;
     Index : out Natural
   ) return Paging_Process_Result
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Get_Page_Table_Index
   --
   --  Purpose:
   --    This function gets the index of the page table entry in a page table
   --    corresponding to a particular address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Get_Page_Table_Index (
     Addr  : System.Address;
     Index : out Natural
   ) return Paging_Process_Result
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Individual Page Table type.
   --  This is an array of 1024 indiviudal Pages.
   ----------------------------------------------------------------------------
   type Page_Table is array (Natural range 0 .. 1023) of Page_Frame;

   ----------------------------------------------------------------------------
   --  Page Table array type.
   --  This is an array of 1024 indiviudal Page Tables. This is used to
   --  interface with the externally declared block of memory reserved for
   --  implementing the page tables.
   ----------------------------------------------------------------------------
   type Page_Table_Array is array (Natural range 0 .. 1023) of Page_Table;

   ----------------------------------------------------------------------------
   --  Page Directory array.
   --  This is used to implement the main page table directory.
   ----------------------------------------------------------------------------
   type Page_Directory_Array is array (Natural range 0 .. 1023)
     of Page_Directory_Entry;

   ----------------------------------------------------------------------------
   --  Allocate_Page_Frame
   --
   --  Purpose:
   --    This procedure allocates a page frame.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Allocate_Page_Frame (
     Virtual_Address : System.Address;
     Frame_Address   : out Page_Aligned_Address
   ) return Paging_Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Map_Page_Frame
   --
   --  Purpose:
   --    This procedure maps an individual page frame. This maps an individual
   --    4K aligned virtual address frame to a physical address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Map_Page_Frame (
     Directory        : Page_Directory_Array;
     Physical_Address : System.Address;
     Virtual_Address  : System.Address
   ) return Paging_Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  System Page Directory.
   --  This is the main directory containing all of the page tables.
   ----------------------------------------------------------------------------
   Page_Directory : Page_Directory_Array
   with Import,
     Convention    => Assembler,
     External_Name => "page_directory_start",
     Volatile;

   ----------------------------------------------------------------------------
   --  The System's Page Tables.
   --  This is implemented by reserving space in the linker script, which we
   --  treat as an array of 1024 page tables.
   ----------------------------------------------------------------------------
   Page_Tables : Page_Table_Array
   with Import,
     Convention    => Assembler,
     External_Name => "page_tables_start",
     Volatile;

   ----------------------------------------------------------------------------
   --  The Kernel page directory.
   --  Used for identity mapping the kernel after boot.
   ----------------------------------------------------------------------------
   Kernel_Page_Directory : Page_Directory_Array
   with Export,
     Convention    => Assembler,
     External_Name => "kernel_page_directory",
     Volatile;

end x86.Memory;
