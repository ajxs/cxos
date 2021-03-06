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
     Unhandled_Exception,
     Unset
   );

   ----------------------------------------------------------------------------
   --  Get_Stack_Top
   --
   --  Purpose:
   --    This function returns a pointer to the currently loaded task's stack
   --    top in virtual memory.
   ----------------------------------------------------------------------------
   function Get_Stack_Top return System.Address
   with Import,
     Convention    => Assembler,
     External_Name => "cxos_memory_get_stack_top";

   ----------------------------------------------------------------------------
   --  Allocate_Kernel_Memory
   ----------------------------------------------------------------------------
   procedure Allocate_Kernel_Memory (
     Size   :     Positive;
     Addr   : out System.Address;
     Status : out Process_Result
   );

private
   ----------------------------------------------------------------------------
   --  The size of the kernel stack, in bytes.
   ----------------------------------------------------------------------------
   KERNEL_STACK_SIZE : constant := 16#4000#;

   ----------------------------------------------------------------------------
   --  The size of the kernel secondary stack, in bytes.
   ----------------------------------------------------------------------------
   KERNEL_SECONDARY_STACK_SIZE : constant := 16#4000#;

end Cxos.Memory;
