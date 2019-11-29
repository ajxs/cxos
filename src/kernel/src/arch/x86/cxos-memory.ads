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

-------------------------------------------------------------------------------
--  CXOS.MEMORY
--
--  Purpose:
--    This package contains code and defintions for working with memory.
-------------------------------------------------------------------------------
package Cxos.Memory is
   pragma Preelaborate;

   use Interfaces;

   ----------------------------------------------------------------------------
   --  Process Result type.
   --  Used for storing and returning the result of an internal memory related
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
     Memory_Map_Not_Present,
     Success,
     Unhandled_Exception
   );

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    Initialises the kernel's memory management system.
   --    Performs the initial kernel memory mapping.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Initialise return Process_Result;

private
   ----------------------------------------------------------------------------
   --  Kernel Page Directory Address.
   --  This contains the physical address of the kernel page directory.
   ----------------------------------------------------------------------------
   Kernel_Page_Directory_Addr : System.Address;

   ----------------------------------------------------------------------------
   --  Clear_Boot_Page_Structures
   --
   --  Purpose:
   --    This procedure marks the boot page structures as being unallocated
   --    memory, free for being overwritten.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Clear_Boot_Page_Structures return Process_Result;

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
   function Initialise_Kernel_Page_Directory return Process_Result;

   ----------------------------------------------------------------------------
   --  Map_Vga_Memory
   --
   --  Purpose:
   --    Maps VGA memory to a usable virtual memory address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Map_Vga_Memory return Process_Result;

   ----------------------------------------------------------------------------
   --  Mark_Low_Memory
   --
   --  Purpose:
   --    This procedure marks all memory below 1M as being non-free in the
   --    kernel memory map.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Mark_Low_Memory return Process_Result;

   ----------------------------------------------------------------------------
   --  Mark_Kernel_Memory
   --
   --  Purpose:
   --    This procedure marks the memory used by the kernel as being allocated
   --    and non-free in the memory map.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Mark_Kernel_Memory return Process_Result;

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Memory_Map
   --
   --  Purpose:
   --    Parses the multiboot memory map structures, mapping the specified
   --    memory regions listed in the multiboot structure.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Parse_Multiboot_Memory_Map (
     Memory_Map_Addr   : System.Address;
     Memory_Map_Length : Unsigned_32
   );
end Cxos.Memory;
