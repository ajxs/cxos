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

with Cxos.Serial;
with Cxos.Time_Keeping;

package body Cxos.Process is
   ----------------------------------------------------------------------------
   --  Create_Task
   ----------------------------------------------------------------------------
   function Create_Process return Process_Result is
   begin
      Allocate_Paging_Structures :
         begin
            null;
         end Allocate_Paging_Structures;

      Map_Kernel :
         begin
            null;
         end Map_Kernel;

      return Success;
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

   procedure Initialise is
   begin
      Allocate_Address :
         begin
            null;
         end Allocate_Address;
   end Initialise;
end Cxos.Process;
