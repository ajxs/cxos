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

with Interfaces; use Interfaces;
with System;

-------------------------------------------------------------------------------
--  CXOS.MEMORY.MAP
--
--  Purpose:
--    This package contains code and defintions for working with a map of
--    usable memory on the system.
-------------------------------------------------------------------------------
package Cxos.Memory.Map is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Memory Map Frame type.
   --  Represents the presence of an individual page frame in memory.
   ----------------------------------------------------------------------------
   type Memory_Map_Frame_State is (
     Allocated,
     Unallocated
   )
   with Size => 1;
   for Memory_Map_Frame_State use (
     Allocated   => 0,
     Unallocated => 1
   );

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the system memory map.
   --    Every page frame is initialised as being allocated and non-free.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise;

   ----------------------------------------------------------------------------
   --  Allocate_Frames
   --
   --  Purpose:
   --    Finds and allocates free page frames, returning a pointer to the
   --    start of the free memory block returned.
   --    This procedure will search for the requested number of contiguous
   --    frames of free memory.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Allocate_Frames (
     Addr   : out System.Address;
     Status : out Process_Result;
     Count  :     Natural := 1
   );

   ----------------------------------------------------------------------------
   --  Mark_Memory_Range
   --
   --  Purpose:
   --    This function marks a memory range as being either allocated or free.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Mark_Memory_Range (
     Base   : System.Address;
     Length : Unsigned_32;
     Status : Memory_Map_Frame_State
   ) return Process_Result;

private
   ----------------------------------------------------------------------------
   --  Find_Free_Frames
   --
   --  Purpose:
   --    This function returns the index of the first occurrence of the
   --    specified number of contiguous free frames.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Find_Free_Frames (
     Index  : out Natural;
     Count  :     Natural := 1;
     Status : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Set_Frame_State
   --
   --  Purpose:
   --    This procedure sets the status of a memory frame at a particular
   --    address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Set_Frame_State (
     Addr   :     System.Address;
     State  :     Memory_Map_Frame_State;
     Status : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Set_Frame_State
   --
   --  Purpose:
   --    This procedure sets the status of a memory frame at a particular
   --    index in the frame array.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Set_Frame_State (
     Index  :     Natural;
     State  :     Memory_Map_Frame_State;
     Status : out Process_Result
   );

   ----------------------------------------------------------------------------
   --  Get_Frame_Address
   --
   --  Purpose:
   --    This function gets the physical memory address corresponding to an
   --    individual memory map frame.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Get_Frame_Address (
     Index :     Natural;
     Addr  : out System.Address
   ) return Process_Result
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Get_Frame_Index
   --
   --  Purpose:
   --    This function gets the index into the memory map of the frame
   --    corresponding to a particular address.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Get_Frame_Index (
     Addr  : System.Address;
     Index : out Natural
   ) return Process_Result
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  Memory Map Array type.
   --  Represents all page frames across the full linear address space.
   ----------------------------------------------------------------------------
   type Memory_Map_Array is array (Natural range 0 .. 16#100000#)
     of Memory_Map_Frame_State
   with Pack;

   ----------------------------------------------------------------------------
   --  The system memory map.
   ----------------------------------------------------------------------------
   Memory_Map : Memory_Map_Array := (others => Unallocated);

end Cxos.Memory.Map;
