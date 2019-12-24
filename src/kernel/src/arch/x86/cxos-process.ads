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
         Id           : Integer;
         Stack_Top    : System.Address;
         Page_Dir_Ptr : System.Address;
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
      Process_Block : out Process_Control_Block
   ) return Process_Result;

private
   ----------------------------------------------------------------------------
   --  Array type to contain the currently loaded system processes.
   ----------------------------------------------------------------------------
   type Process_Control_Block_Array is
     array (Natural range 0 .. 1023) of Process_Control_Block;

   ----------------------------------------------------------------------------
   --  The id of the currently running system processes.
   ----------------------------------------------------------------------------
   Current_Process : Natural;

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

end Cxos.Process;
