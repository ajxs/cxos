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

with Cxos.Memory;
with Cxos.Memory.Paging;
with Cxos.Serial;
with Cxos.Time_Keeping;

package body Cxos.Process is
   ----------------------------------------------------------------------------
   --  Create_Task
   ----------------------------------------------------------------------------
   function Create_Process (
      Process_Block : out Process_Control_Block
   ) return Process_Result is
      --  The newly allocated page directory to map the virtual address
      --  space for the newly created process.
      Page_Dir_Addr : System.Address;
   begin
      --  Allocate the page directory for the newly created process.
      Allocate_Page_Directory :
         declare
            use Cxos.Memory;

            --  The result of allocating the new page directory.
            Allocate_Result : Cxos.Memory.Process_Result;
         begin
            Allocate_Result := Cxos.Memory.Paging.
              Create_New_Page_Directory (Page_Dir_Addr);
            if Allocate_Result /= Success then
               return Unhandled_Exception;
            end if;
         end Allocate_Page_Directory;

      --  Allocate the process control block.
      Allocate_Structure :
         begin
            Process_Block.Page_Dir_Ptr := Page_Dir_Addr;
            Process_Block.Id := 337;
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
      Test_Result : Process_Result;
   begin
      Create_Idle_Process :
         begin
            Test_Result := Create_Process (Test_Block);
            if Test_Result /= Success then
               Cxos.Serial.Put_String ("Error" & ASCII.LF);
            end if;
            Cxos.Serial.Put_String ("Allocated process: " &
              Test_Block.Id'Image & ASCII.LF);
         end Create_Idle_Process;
   exception
      when Constraint_Error =>
         null;
   end Initialise;
end Cxos.Process;
