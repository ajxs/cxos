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

with System.Storage_Elements; use System.Storage_Elements;

package body Cxos.Memory.Map is
   ----------------------------------------------------------------------------
   --  Allocate_Frames
   ----------------------------------------------------------------------------
   procedure Allocate_Frames (
     Addr   : out System.Address;
     Status : out Process_Result;
     Count  :     Natural := 1
   ) is
      --  The result of subprocedure calls.
      Result : Process_Result;
      --  The index of the found free frames.
      Index  : Natural;
   begin
      Find_Free_Frames (Index, Count, Status);
      if Status /= Success then
         return;
      end if;

      --  Output address is set here.
      Result := Get_Frame_Address (Index, Addr);
      if Result /= Success then
         Status := Result;
         return;
      end if;

      Set_Frame_State (Index, Allocated, Status);
      if Status /= Success then
         return;
      end if;

      Status := Success;
   exception
      when Constraint_Error =>
         Status := Invalid_Address_Argument;
   end Allocate_Frames;

   ----------------------------------------------------------------------------
   --  Find_Free_Frames
   ----------------------------------------------------------------------------
   procedure Find_Free_Frames (
     Index  : out Natural;
     Count  :     Natural := 1;
     Status : out Process_Result
   ) is
   begin
      --  Loop through each frame in the map until we find the requested amount
      --  of contiguous unallocated frames.
      for Curr_Idx in Memory_Map'Range loop
         Check_Frame :
            begin
               if Memory_Map (Curr_Idx) = Unallocated then
                     --  If only one frame is needed, don't do any range check.
                     if Count = 1 then
                        Index := Curr_Idx;

                        Status := Success;
                        return;
                     end if;

                     --  If more than one contiguous frame is required, test
                     --  the range.
                     Test_Count :
                        declare
                           --  The upper bound of the range of adjacent frames
                           --  to test.
                           Test_Range : Natural;
                        begin
                           Test_Range := Count - 1;

                           for Count_Idx in Natural range 1 .. Test_Range loop
                              --  Exit if we're under the desired length and
                              --  hit an allocated frame.
                              exit when
                                Memory_Map (Curr_Idx + Count_Idx) = Allocated;

                              --  If the count of contiguous unallocated frames
                              --  is equal to the upper bound of the test range
                              --  then we've found the right number of adjacent
                              --  free frames.
                              if Count_Idx = Test_Range then
                                 Index := Curr_Idx;

                                 Status := Success;
                                 return;
                              end if;
                           end loop;
                        exception
                           when Constraint_Error =>
                              --  If we overflow the total number of frames,
                              --  abort, since there is clearly not the desired
                              --  count of free frames.
                              Status := No_Free_Frames;
                              return;
                        end Test_Count;
               end if;
            exception
               when Constraint_Error =>
                  --  In this case, assume it is used.
                  null;
            end Check_Frame;
      end loop;

      Status := No_Free_Frames;
   end Find_Free_Frames;

   ----------------------------------------------------------------------------
   --  Get_Frame_Address
   ----------------------------------------------------------------------------
   function Get_Frame_Address (
     Index :     Natural;
     Addr  : out System.Address
   ) return Process_Result is
   begin
      Addr := To_Address (Integer_Address (Index * 16#1000#));

      return Success;
   exception
      when Constraint_Error =>
         return Invalid_Index_Argument;
   end Get_Frame_Address;

   ----------------------------------------------------------------------------
   --  Get_Frame_Index
   ----------------------------------------------------------------------------
   function Get_Frame_Index (
     Addr  : System.Address;
     Index : out Natural
   ) return Process_Result is
   begin
      Index := Natural (To_Integer (Addr) / 16#1000#);

      if not Index'Valid then
         raise Constraint_Error;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Invalid_Address_Argument;
   end Get_Frame_Index;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      for Frame of Memory_Map loop
         Frame := Allocated;
      end loop;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Mark_Memory_Range
   ----------------------------------------------------------------------------
   function Mark_Memory_Range (
     Base   : System.Address;
     Length : Unsigned_32;
     Status : Memory_Map_Frame_State
   ) return Process_Result is
      --  The page aligned address of the current memory frame.
      Curr_Frame_Addr  : Unsigned_32;
      --  The number of frames within this memory region.
      Frame_Count      : Unsigned_32;
      --  The result of the frame status set process.
      Set_Frame_Result : Process_Result;
   begin
      --  Calculate the amount of frames within this memory range.
      Frame_Count := 1 + (Length / 16#1000#);

      --  Set the initial frame address to a page aligned base address.
      Curr_Frame_Addr :=
        Unsigned_32 (To_Integer (Base)) and 16#FFFFF000#;

      --  Iterate over each frame in this range, setting its individual status.
      for I in 0 .. Frame_Count loop
         Set_Frame_State (To_Address (Integer_Address (Curr_Frame_Addr)),
           Status, Set_Frame_Result);

         if Set_Frame_Result /= Success then
            return Set_Frame_Result;
         end if;

         --  Increment the current frame address by the size of a single frame.
         Curr_Frame_Addr := Curr_Frame_Addr + 16#1000#;
      end loop;

      return Success;
   exception
      when Constraint_Error =>
         return Invalid_Address_Argument;
   end Mark_Memory_Range;

   ----------------------------------------------------------------------------
   --  Set_Frame_State
   ----------------------------------------------------------------------------
   procedure Set_Frame_State (
     Addr   :     System.Address;
     State  :     Memory_Map_Frame_State;
     Status : out Process_Result
   ) is
      Map_Idx : Natural := 0;
   begin
      Get_Address_Map_Index :
         begin
            Status := Get_Frame_Index (Addr, Map_Idx);
            if Status /= Success then
               return;
            end if;
         exception
            when Constraint_Error =>
               Status := Invalid_Address_Argument;
               return;
         end Get_Address_Map_Index;

      Set_Frame_Use_State :
         begin
            Memory_Map (Map_Idx) := State;
         exception
            when Constraint_Error =>
               Status := Invalid_Address_Argument;
               return;
         end Set_Frame_Use_State;

         Status := Success;
   end Set_Frame_State;

   ----------------------------------------------------------------------------
   --  Set_Frame_State
   ----------------------------------------------------------------------------
   procedure Set_Frame_State (
     Index  :     Natural;
     State  :     Memory_Map_Frame_State;
     Status : out Process_Result
   ) is
   begin
      Memory_Map (Index) := State;

      Status := Success;
   exception
      when Constraint_Error =>
         Status := Invalid_Index_Argument;
   end Set_Frame_State;

end Cxos.Memory.Map;
