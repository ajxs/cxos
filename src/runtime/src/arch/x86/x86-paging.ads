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
--  SYSTEM.X86.PAGING
--
--  Purpose:
--    This package contains code and defintions for implementing and working
--    with paging on the x86 platform.
-------------------------------------------------------------------------------
package x86.Paging is
   pragma Preelaborate (x86.Paging);

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure iniitalises paging on the processor.
   --    This initialises the page directory and table structures and loads
   --    their location into the processor.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise;

   ----------------------------------------------------------------------------
   --  Finalise
   --
   --  Purpose:
   --    This procedure finalises the loading of the page directory structures
   --    into the processor's control registers.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Finalise
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
     Invalid_Argument,
     Invalid_Non_Aligned_Address,
     Invalid_Table_Index,
     Frame_Allocation_Error,
     Success
   );

   ----------------------------------------------------------------------------
   --  Type to hold a 20bit address.
   --  These are truncated 32bit addresses that are assumed to be 4K aligned,
   --  as such there is no need to hold the lower-order 12bits.
   ----------------------------------------------------------------------------
   type Aligned_Address is mod 2 ** 20;

   ----------------------------------------------------------------------------
   --  Type for the address of a page table.
   --  Used by a Page Directory Entry.
   ----------------------------------------------------------------------------
   type Page_Table_Address is new Aligned_Address;

   ----------------------------------------------------------------------------
   --  Type to hold the physical address of a page.
   --  Used by a Page Table Entry.
   ----------------------------------------------------------------------------
   type Physical_Page_Address is new Aligned_Address;

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
         Table_Address : Page_Table_Address;
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
   --  Page Table Entry type.
   ----------------------------------------------------------------------------
   type Page_Table_Entry is
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
         Page_Address : Physical_Page_Address;
      end record
   with Size => 32;
   for Page_Table_Entry use
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
   --  Check_Address_Aligned
   --
   --  Purpose:
   --    Checks whether a provided address is 4K aligned.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Check_Address_Aligned (
     Addr : System.Address
   ) return Boolean
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Convert_To_Aligned_Address
   --
   --  Purpose:
   --    This function converts a System Address to the 20bit 4kb aligned
   --    address type expected by the page table entities.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Convert_To_Aligned_Address (
     Addr : System.Address
   ) return Aligned_Address
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
     Addr : Aligned_Address
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
     Addr    : System.Address;
     Index   : out Natural
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
     Addr    : System.Address;
     Index   : out Natural
   ) return Paging_Process_Result
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Individual Page Table type.
   --  This is an array of 1024 indiviudal Page Table Entries.
   ----------------------------------------------------------------------------
   type Page_Table is array (Natural range 0 .. 1023) of Page_Table_Entry;

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
   --  Map_Page_Table_Entry
   --
   --  Purpose:
   --    This procedure maps a specific page table entry to a virtual address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Map_Page_Table_Entry (
     Directory        : in out Page_Directory_Array;
     Physical_Address : System.Address;
     Virtual_Address  : System.Address
   ) return Paging_Process_Result;

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

end x86.Paging;
