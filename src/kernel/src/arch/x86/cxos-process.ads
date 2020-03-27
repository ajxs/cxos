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
      end record
   with Size => 96;
   for Process_Control_Block use
      record
         Id  at 0 range 0 .. 31;
         ESP at 4 range 0 .. 31;
         CR3 at 8 range 0 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  Process Result
   --  Used for storing and returning the result of an internal process
   --  procedure.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Failure,
     Success,
     Unhandled_Exception
   );

   ----------------------------------------------------------------------------
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
   --  Switch_To_Process
   --
   --  Purpose:
   --    fsfsfs
   ----------------------------------------------------------------------------
   procedure Switch_To_Process (
     Target_Process : Process_Control_Block
   );

private
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
   Current_Process : Natural := 0;

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
