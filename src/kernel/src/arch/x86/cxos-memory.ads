------------------------------------------------------------------------------
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
--  CXOS.MEMORY
--
--  Purpose:
--    This package contains code and defintions for working with memory.
-------------------------------------------------------------------------------
package Cxos.Memory is
   pragma Preelaborate;

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
     Invalid_Address_Argument,
     Invalid_Index_Argument,
     No_Free_Frames,
     Memory_Map_Not_Present,
     Success,
     Unhandled_Exception
   );

   ----------------------------------------------------------------------------
   --  Get_Stack_Top
   --
   --  Purpose:
   --    This function returns a pointer to the currently loaded task's stack
   --    top.
   ----------------------------------------------------------------------------
   function Get_Stack_Top return System.Address
   with Import,
     Convention    => Assembler,
     External_Name => "cxos_memory_get_stack_top";

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

end Cxos.Memory;
