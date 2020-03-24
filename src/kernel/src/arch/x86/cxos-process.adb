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
      Process_Block.Id           := 0;

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
   begin
      --  Allocate the page directory for the newly created process.
      Allocate_Page_Directory :
         declare
            use Cxos.Memory;

            --  The result of allocating the new page directory.
            Allocate_Result : Cxos.Memory.Process_Result;
         begin
            Allocate_Result := Cxos.Memory.Paging.
              Create_New_Address_Space (Page_Dir_Addr, Idle'Address);
            if Allocate_Result /= Success then
               Cxos.Serial.Put_String ("Error allocating new address block" &
                 ASCII.LF);
               return Unhandled_Exception;
            end if;
         end Allocate_Page_Directory;

      --  Allocate the process control block.
      Allocate_Structure :
         declare
            use System.Storage_Elements;
         begin
            Process_Block.Page_Dir_Ptr := Page_Dir_Addr;
            Process_Block.Stack_Top    := To_Address (16#FF000FE8#);
            Process_Block.Id           := 337;
         end Allocate_Structure;

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

      Start_Time : Cxos.Time_Keeping.Time;
   begin
      Start_Time := Cxos.Time_Keeping.Clock;

      loop
         Cxos.Serial.Put_String ("Idling" & ASCII.LF);
         Wait_Loop :
            loop
               if (Cxos.Time_Keeping.Clock - Start_Time) > 1000 then
                  exit Wait_Loop;
               end if;
            end loop Wait_Loop;
      end loop;
   end Idle;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
      Test_Block : Process_Control_Block;

      Create_Task_Result : Process_Result;
      Idle_Task_Block    : Process_Control_Block;
   begin
      --  Create the system idle process from the pseudo-process currently
      --  running from boot.
      Create_Idle_Process :
         begin
            Create_Task_Result := Create_Initial_Kernel_Task (Idle_Task_Block);
            if Create_Task_Result /= Success then
               Cxos.Serial.Put_String ("Error creating idle task" & ASCII.LF);
            end if;

            Cxos.Serial.Put_String ("Allocated idle process: " &
              Idle_Task_Block.Id'Image & ASCII.LF);

            Test_Out :
               declare
                  use System.Storage_Elements;

                  CR3 : constant Integer_Address := To_Integer (
                    Idle_Task_Block.Page_Dir_Ptr);
                  ESP : constant Integer_Address := To_Integer (
                    Idle_Task_Block.Stack_Top);
               begin
                  Cxos.Serial.Put_String ("Idle CR3: " & CR3'Image & ASCII.LF);
                  Cxos.Serial.Put_String ("Idle ESP: " & ESP'Image & ASCII.LF);
               end Test_Out;
         end Create_Idle_Process;

      Create_Test_Process :
         begin
            Create_Task_Result := Create_Process (Test_Block);
            if Create_Task_Result /= Success then
               Cxos.Serial.Put_String ("Error" & ASCII.LF);
            end if;
            Cxos.Serial.Put_String ("Allocated process: " &
              Test_Block.Id'Image & ASCII.LF);

            Test_Block_Debug :
               declare
                  use System.Storage_Elements;

                  CR3 : constant Integer_Address := To_Integer (
                    Test_Block.Page_Dir_Ptr);
                  ESP : constant Integer_Address := To_Integer (
                    Test_Block.Stack_Top);
               begin
                  Cxos.Serial.Put_String ("New CR3: " & CR3'Image & ASCII.LF);
                  Cxos.Serial.Put_String ("New ESP: " & ESP'Image & ASCII.LF);
               end Test_Block_Debug;
         end Create_Test_Process;

      Switch_To_Process (Test_Block);
   exception
      when Constraint_Error =>
         null;
   end Initialise;
end Cxos.Process;
