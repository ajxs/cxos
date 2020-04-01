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

with Cxos.Time_Keeping; use Cxos.Time_Keeping;
with Interfaces; use Interfaces;
with System;

-------------------------------------------------------------------------------
--  CXOS.PROCESS
--
--  Purpose:
--    This package contains functionality for working with processes.
-------------------------------------------------------------------------------
package Cxos.Process is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Process Control Block
   --  Used for storing the data necessary for initialising and running
   --  kernel processes.
   ----------------------------------------------------------------------------
   type Process_Control_Block is
      record
         Id  : Integer;
         ESP : System.Address;
         CR3 : System.Address;
         EAX : Unsigned_32;
         EBX : Unsigned_32;
         ECX : Unsigned_32;
         EDX : Unsigned_32;
         EDI : Unsigned_32;
         ESI : Unsigned_32;
         EBP : Unsigned_32;
      end record
   with Size => 320;
   for Process_Control_Block use
      record
         Id  at 0  range 0 .. 31;
         ESP at 4  range 0 .. 31;
         CR3 at 8  range 0 .. 31;
         EAX at 12 range 0 .. 31;
         EBX at 16 range 0 .. 31;
         ECX at 20 range 0 .. 31;
         EDX at 24 range 0 .. 31;
         EDI at 28 range 0 .. 31;
         ESI at 32 range 0 .. 31;
         EBP at 36 range 0 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  Process Result
   --  Used for storing and returning the result of an internal process
   --  procedure.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Failure,
     Success,
     No_Running_Processes,
     Unhandled_Exception
   );

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    Initialises the system's core processes.
   ----------------------------------------------------------------------------
   procedure Initialise;

   ----------------------------------------------------------------------------
   --  Create_Process
   --
   --  Purpose:
   --    Creates the process control block for a new process.
   ----------------------------------------------------------------------------
   function Create_Process (
     Process_Block : out Process_Control_Block;
     Func_Start    :     System.Address
   ) return Process_Result;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Run_Scheduler;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Yield;
private
   ----------------------------------------------------------------------------
   --  Switch_To_Process
   --
   --  Purpose:
   --    fsfsfs
   ----------------------------------------------------------------------------
   procedure Switch_To_Process (
     Old_Process    : Process_Control_Block;
     Target_Process : Process_Control_Block
   );

   ----------------------------------------------------------------------------
   --  Find_Next_Process
   ----------------------------------------------------------------------------
   function Find_Next_Process (
     Next_Process : out Process_Control_Block
   ) return Process_Result;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   Curr_Process_Slice_Start_Time : Time := 0;

   ----------------------------------------------------------------------------
   --  The amount of time that each process is allowed.
   ----------------------------------------------------------------------------
   PROCESS_TIME_SLICE : constant := 100000;

   ----------------------------------------------------------------------------
   --  The system's idle task.
   ----------------------------------------------------------------------------
   Idle_Task : Process_Control_Block;

   ----------------------------------------------------------------------------
   --  Array type to contain the currently loaded system processes.
   ----------------------------------------------------------------------------
   type Process_Control_Block_Array is
     array (Natural range 0 .. 1023) of Process_Control_Block;

   ----------------------------------------------------------------------------
   --  The number of running processes.
   --  This is incremented and decremented as processes are created and
   --  destroyed.
   ----------------------------------------------------------------------------
   Process_Count : Natural := 0;

   ----------------------------------------------------------------------------
   --  The id of the currently running system processes.
   ----------------------------------------------------------------------------
   Current_Process_Id : Natural := 0;

   ----------------------------------------------------------------------------
   --  The currently loaded system processes.
   ----------------------------------------------------------------------------
   System_Processes : Process_Control_Block_Array;

   ----------------------------------------------------------------------------
   --  Idle
   --
   --  Purpose:
   --    This function serves as the system idle process.
   ----------------------------------------------------------------------------
   procedure Idle;

   ----------------------------------------------------------------------------
   --  Create_Initial_Kernel_Task
   --
   --  Purpose:
   --    Creates the kernel idle task.
   ----------------------------------------------------------------------------
   function Create_Initial_Kernel_Task (
     Process_Block : out Process_Control_Block
   ) return Process_Result;

   ----------------------------------------------------------------------------
   --  Print_Process_Block_Info
   --
   --  Purpose:
   --    Prints debugging information for a process block.
   ----------------------------------------------------------------------------
   procedure Print_Process_Block_Info (
     Proc : Process_Control_Block
   );

   ----------------------------------------------------------------------------
   --  Save_Process_State
   --
   --  Purpose:
   --    This function stores the state of the current process.
   ----------------------------------------------------------------------------
   procedure Save_Process_State (
     Curr_Proc : Process_Control_Block
   ) with Import,
     Convention    => Assembler,
     External_Name => "cxos_process_save_process_state";

   ----------------------------------------------------------------------------
   --  Load Process
   --
   --  Purpose:
   --    This is the function which handles switching to a new kernel process.
   ----------------------------------------------------------------------------
   procedure Load_Process (
     Target_Process : Process_Control_Block
   ) with Import,
     Convention    => Assembler,
     External_Name => "cxos_process_load_process";
end Cxos.Process;
