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
with Cxos.Memory;
with Cxos.Memory.Paging;
with Cxos.Serial;
with Cxos.Time_Keeping;

package body Cxos.Process is
   ----------------------------------------------------------------------------
   --  Create_Initial_Kernel_Task
   ----------------------------------------------------------------------------
   function Create_Initial_Kernel_Task (
     Process_Block : out Process_Control_Block
   ) return Process_Result is
   begin
      --  Set the page directory pointer to the currently loaded page
      --  directory pointer.
      Process_Block.Id  := Process_Count;
      Process_Block.CR3 := Cxos.Memory.Paging.Current_Page_Dir_Ptr;
      Process_Block.ESP := Cxos.Memory.Get_Stack_Top;

      --  Increment the process count.
      Increment_Process_Count :
         begin
            Process_Count := Process_Count + 1;
         exception
            when Constraint_Error =>
               Cxos.Serial.Put_String ("Process count exhausted" & ASCII.LF);
               return Unhandled_Exception;
         end Increment_Process_Count;

      return Success;
   end Create_Initial_Kernel_Task;

   ----------------------------------------------------------------------------
   --  Create_Task
   ----------------------------------------------------------------------------
   function Create_Process (
     Process_Block : out Process_Control_Block;
     Func_Start    :     System.Address
   ) return Process_Result is
      use System.Storage_Elements;

      --  The newly allocated page directory to map the virtual address
      --  space for the newly created process.
      Page_Dir_Addr     : System.Address;
   begin
      --  Allocate the page directory for the newly created process.
      Allocate_Page_Directory :
         declare
            use Cxos.Memory;

            --  The result of allocating the new page directory.
            Allocate_Result : Cxos.Memory.Process_Result;
         begin
            Allocate_Result := Cxos.Memory.Paging.
              Create_New_Address_Space (Page_Dir_Addr, Func_Start);
            if Allocate_Result /= Success then
               Cxos.Serial.Put_String ("Error allocating new address block" &
                 ASCII.LF);
               return Unhandled_Exception;
            end if;
         end Allocate_Page_Directory;

      --  Allocate the process control block.
      Allocate_Structure :
         begin
            Process_Block.Id  := Process_Count;
            Process_Block.CR3 := Page_Dir_Addr;
            Process_Block.ESP := To_Address (16#FF003FE8#);
         end Allocate_Structure;

      --  Increment the process count.
      Increment_Process_Count :
         begin
            Process_Count := Process_Count + 1;
         exception
            when Constraint_Error =>
               Cxos.Serial.Put_String ("Process count exhausted" & ASCII.LF);
               return Unhandled_Exception;
         end Increment_Process_Count;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Create_Process;

   ----------------------------------------------------------------------------
   --  Idle
   ----------------------------------------------------------------------------
   procedure Idle is
      use Cxos.Time_Keeping;

      --  The start of the idle cycle.
      Start_Time : Cxos.Time_Keeping.Time;
   begin
      Start_Time := Cxos.Time_Keeping.Clock;

      loop
         Cxos.Serial.Put_String ("Idling" & ASCII.LF);

         --  Wait a predetermined amount of time.
         Wait_Loop :
            loop
               if (Cxos.Time_Keeping.Clock - Start_Time) > 100000 then
                  exit Wait_Loop;
               end if;
            end loop Wait_Loop;

            --  Reset start.
            Start_Time := Cxos.Time_Keeping.Clock;
      end loop;
   end Idle;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
      Test_Block : Process_Control_Block;

      --  The result status code of internal processes.
      Result : Process_Result;
   begin
      --  Create the system idle process from the pseudo-process currently
      --  running from boot.
      Create_Idle_Process :
         begin
            Result := Create_Initial_Kernel_Task (Idle_Task);
            if Result /= Success then
               Cxos.Serial.Put_String ("Error creating idle task" & ASCII.LF);
            end if;

            Print_Process_Block_Info (Idle_Task);
         end Create_Idle_Process;

      Create_Test_Process :
         begin
            Result := Create_Process (Test_Block, Idle'Address);
            if Result /= Success then
               Cxos.Serial.Put_String ("Error" & ASCII.LF);
            end if;

            Print_Process_Block_Info (Test_Block);
         end Create_Test_Process;

         Switch_To_Process (Test_Block);
   exception
      when Constraint_Error =>
         null;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Print_Process_Block_Info
   ----------------------------------------------------------------------------
   procedure Print_Process_Block_Info (
     Proc : Process_Control_Block
   ) is
      use System.Storage_Elements;

      CR3 : constant Integer_Address := To_Integer (Proc.CR3);
      ESP : constant Integer_Address := To_Integer (Proc.ESP);
   begin
      Cxos.Serial.Put_String ("Process Id: " & Proc.Id'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  CR3: " & CR3'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  ESP: " & ESP'Image & ASCII.LF);
      Cxos.Serial.Put_String ("------------------------" & ASCII.LF);
   end Print_Process_Block_Info;

   ----------------------------------------------------------------------------
   --  Switch_To_Process
   ----------------------------------------------------------------------------
   procedure Switch_To_Process (
     Target_Process : Process_Control_Block
   ) is
      --  The currently running process.
      Curr_Proc : Process_Control_Block;
   begin
      Curr_Proc := System_Processes (Current_Process);
      pragma Unreferenced (Curr_Proc);

      Store_Current_Process :
         begin
            null;
         end Store_Current_Process;

      Load_Process (Target_Process);
   exception
      when Constraint_Error =>
         null;
   end Switch_To_Process;

end Cxos.Process;
