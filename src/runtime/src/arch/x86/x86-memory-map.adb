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

with System.Storage_Elements;

package body x86.Memory.Map is
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Allocate_Frame
   ----------------------------------------------------------------------------
   function Allocate_Frame (
     Addr : out System.Address
   ) return Memory_Map_Process_Result is
      Process_Result : Memory_Map_Process_Result;
      Index          : Natural;
   begin
      Process_Result := Find_Free_Frame (Index);
      if Process_Result /= Success then
         return Process_Result;
      end if;

      Process_Result := Get_Frame_Address (Index, Addr);
      if Process_Result /= Success then
         return Process_Result;
      end if;

      Process_Result := Set_Frame_State (Index, False);
      if Process_Result /= Success then
         return Process_Result;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Invalid_Address_Argument;
   end Allocate_Frame;

   ----------------------------------------------------------------------------
   --  Find_Free_Frame
   ----------------------------------------------------------------------------
   function Find_Free_Frame (
     Index : out Natural
   ) return Memory_Map_Process_Result is
   begin
      --  Loop through each frame in the map until we find one that
      --  is unallocated.
      for Current_Index in Memory_Map'Range loop
         Check_Frame :
            begin
               if not Memory_Map (Current_Index) then
                  Index := Current_Index;

                  return Success;
               end if;
            exception
               when Constraint_Error =>
                  --  In this case, assume it is used.
                  null;
            end Check_Frame;
      end loop;

      return No_Free_Frames;
   end Find_Free_Frame;

   ----------------------------------------------------------------------------
   --  Get_Frame_Address
   ----------------------------------------------------------------------------
   function Get_Frame_Address (
     Index : Natural;
     Addr  : out System.Address
   ) return Memory_Map_Process_Result is
   begin
      Addr := To_Address (Integer_Address (Index * 16#1000#));

      return Success;
   exception
      when Constraint_Error =>
         return Invalid_Address_Argument;
   end Get_Frame_Address;

   ----------------------------------------------------------------------------
   --  Get_Frame_Index
   ----------------------------------------------------------------------------
   function Get_Frame_Index (
     Addr  : System.Address;
     Index : out Natural
   ) return Memory_Map_Process_Result is
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
         Frame := False;
      end loop;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Set_Frame_State
   ----------------------------------------------------------------------------
   function Set_Frame_State (
     Addr  : System.Address;
     State : Boolean
   ) return Memory_Map_Process_Result is
      Map_Idx        : Natural;
      Process_Result : Memory_Map_Process_Result;
   begin
      Get_Address_Map_Index :
         begin
            Process_Result := Get_Frame_Index (Addr, Map_Idx);
            if Process_Result /= Success then
               return Process_Result;
            end if;
         exception
            when Constraint_Error =>
               return Invalid_Address_Argument;
         end Get_Address_Map_Index;

      Set_Frame_Use_State :
         begin
            Memory_Map (Map_Idx) := Memory_Map_Frame_State (State);
         exception
            when Constraint_Error =>
               return Invalid_Address_Argument;
         end Set_Frame_Use_State;

         return Success;
   end Set_Frame_State;

   ----------------------------------------------------------------------------
   --  Set_Frame_State
   ----------------------------------------------------------------------------
   function Set_Frame_State (
     Index : Natural;
     State : Boolean
   ) return Memory_Map_Process_Result is
   begin
      Memory_Map (Index) := Memory_Map_Frame_State (State);

      return Success;
   exception
      when Constraint_Error =>
         return Invalid_Address_Argument;
   end Set_Frame_State;

end x86.Memory.Map;
