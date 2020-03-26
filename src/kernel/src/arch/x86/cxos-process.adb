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
      Process_Block.Page_Dir_Ptr := Cxos.Memory.Paging.Current_Page_Dir_Ptr;
      Process_Block.Stack_Top    := Cxos.Memory.Get_Stack_Top;
      Process_Block.Id           := Process_Count;

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
      Process_Block : out Process_Control_Block
   ) return Process_Result is
      --  The newly allocated page directory to map the virtual address
      --  space for the newly created process.
      Page_Dir_Addr     : System.Address;

      Kernel_Stack_Addr : System.Address;
   begin
      --  Allocate the page directory for the newly created process.
      Allocate_Page_Directory :
         declare
            use Cxos.Memory;

            --  The result of allocating the new page directory.
            Allocate_Result : Cxos.Memory.Process_Result;
         begin
            Allocate_Result := Cxos.Memory.Paging.
              Create_New_Address_Space (Page_Dir_Addr);
            if Allocate_Result /= Success then
               Cxos.Serial.Put_String ("Error allocating new address block" &
                 ASCII.LF);
               return Unhandled_Exception;
            end if;
         end Allocate_Page_Directory;

      Allocate_Kernel_Stack :
         declare
            use Cxos.Memory;

            --  The result of allocating the new page directory.
            Allocate_Result : Cxos.Memory.Process_Result;
         begin
            Allocate_Result := Cxos.Memory.
              Create_New_Kernel_Stack (Kernel_Stack_Addr, Idle'Address);
            if Allocate_Result /= Success then
               Cxos.Serial.Put_String ("Error allocating kernel stack" &
                 ASCII.LF);
               return Unhandled_Exception;
            end if;
         end Allocate_Kernel_Stack;

      --  Allocate the process control block.
      Allocate_Structure :
         begin
            Process_Block.Page_Dir_Ptr := Page_Dir_Addr;
            Process_Block.Stack_Top    := Kernel_Stack_Addr;
            Process_Block.Id           := Process_Count;
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

      Create_Task_Result : Process_Result;
   begin
      --  Create the system idle process from the pseudo-process currently
      --  running from boot.
      Create_Idle_Process :
         begin
            Create_Task_Result := Create_Initial_Kernel_Task (Idle_Task);
            if Create_Task_Result /= Success then
               Cxos.Serial.Put_String ("Error creating idle task" & ASCII.LF);
            end if;

            Cxos.Serial.Put_String ("Allocated idle process: " &
              Idle_Task.Id'Image & ASCII.LF);

            Print_Process_Block_Info (Idle_Task);
         end Create_Idle_Process;

      Create_Test_Process :
         begin
            Create_Task_Result := Create_Process (Test_Block);
            if Create_Task_Result /= Success then
               Cxos.Serial.Put_String ("Error" & ASCII.LF);
            end if;
            Cxos.Serial.Put_String ("Allocated process: " &
              Test_Block.Id'Image & ASCII.LF);

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

      CR3 : constant Integer_Address := To_Integer (Proc.Page_Dir_Ptr);
      ESP : constant Integer_Address := To_Integer (Proc.Stack_Top);
   begin
      Cxos.Serial.Put_String ("------------------------" & ASCII.LF);
      Cxos.Serial.Put_String ("Process Id: " & Proc.Id'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  CR3: " & CR3'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  ESP: " & ESP'Image & ASCII.LF);
   end Print_Process_Block_Info;

end Cxos.Process;
