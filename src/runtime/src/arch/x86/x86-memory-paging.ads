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
--  SYSTEM.X86.MEMORY.PAGING
--
--  Purpose:
--    This package contains code and defintions for implementing and working
--    with paging on the x86 platform.
-------------------------------------------------------------------------------
package x86.Memory.Paging is
   pragma Preelaborate (x86.Memory.Paging);

   ----------------------------------------------------------------------------
   --  Paging Process Result
   --  Used for storing and returning the result of an internal paging
   --  procedure.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Frame_Allocation_Error,
     Frame_Not_Allocated,
     Invalid_Argument,
     Invalid_Non_Aligned_Address,
     Invalid_Page_Directory,
     Invalid_Table_Index,
     Invalid_Value,
     Success,
     Table_Not_Allocated
   );

   ----------------------------------------------------------------------------
   --  Kernel Page Directory Address.
   --  This contains the physical address of the kernel page directory.
   ----------------------------------------------------------------------------
   Kernel_Page_Directory_Addr : System.Address;

   ----------------------------------------------------------------------------
   --  Initialise_Kernel_Page_Directory
   --
   --  Purpose:
   --    This function initialises the kernel's main page directory.
   --    This populates the package-visible 'Kernel_Page_Directory_Addr'
   --    variable.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Initialise_Kernel_Page_Directory return Kernel_Process_Result;

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

private
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
   --  Flush_Tlb
   --
   --  Purpose:
   --    Flushes and reloads the Translation Lookaside Buffer.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Flush_Tlb
   with Import,
     Convention    => Assembler,
     External_Name => "__flush_tlb";

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
     Addr  :     System.Address;
     Index : out Natural
   ) return Process_Result
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
     Addr  :     System.Address;
     Index : out Natural
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
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Individual Page Table type.
   --  This is an array of 1024 indiviudal Pages.
   ----------------------------------------------------------------------------
   type Page_Table is array (Natural range 0 .. 1023) of Page_Frame;

   ----------------------------------------------------------------------------
   --  Page Directory.
   --  This type represents a a page directory, an array of individual entries.
   ----------------------------------------------------------------------------
   type Page_Directory is array (Natural range 0 .. 1023)
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
     Virtual_Address :     System.Address;
     Frame_Address   : out Page_Aligned_Address
   ) return Process_Result
   with Volatile_Function;

   ----------------------------------------------------------------------------
   --  Initialise_Page_Directory
   --
   --  Purpose:
   --    This initialises an individual Page Directory.
   --    It will initialise every entry in the directory as being non-present.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Initialise_Page_Directory (
     Page_Dir : in out Page_Directory
   ) return Process_Result;

   ----------------------------------------------------------------------------
   --  Initialise_Page_Table
   --
   --  Purpose:
   --    This initialises an individual Page Table.
   --    The Table will be initialised with all entries marked as non-present.
   --  Excecptions:
   --    None.
   ----------------------------------------------------------------------------
   function Initialise_Page_Table (
     Table : in out Page_Table
   ) return Process_Result;

end x86.Memory.Paging;
