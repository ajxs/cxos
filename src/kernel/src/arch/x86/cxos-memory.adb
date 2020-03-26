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
with Cxos.Memory.Map;
with Cxos.Memory.Paging;
with Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package body Cxos.Memory is
   use Interfaces;

   ----------------------------------------------------------------------------
   --  Create_New_Kernel_Stack
   ----------------------------------------------------------------------------
   function Create_New_Kernel_Stack (
     Stack_Addr  : out System.Address;
     Initial_EIP :     System.Address
   ) return Process_Result is
      --  Virtual address of the stack's top frame's temporary mapping into
      --  the current address space. Used during initialisation.
      Stack_Top_Virt_Addr : System.Address;
      --  Result of internal operations.
      Result : Process_Result;
   begin
      Allocate_Stack_Memory :
         declare
            --  The number of page frames that make up the kernel stack.
            Stack_Frame_Count : Natural;
            --  The address of the top stack frame.
            Stack_Top_Addr    : System.Address;
         begin
            Stack_Frame_Count := KERNEL_STACK_SIZE / 16#1000#;

            --  Allocate page frames for the new stack frame.
            Result := Cxos.Memory.Map.Allocate_Frames (Stack_Addr,
              Stack_Frame_Count);
            if Result /= Success then
               return Result;
            end if;

            Stack_Top_Addr := To_Address (To_Integer (Stack_Addr) +
              (KERNEL_STACK_SIZE - 16#1000#));

            --  Stack_Top_Addr := Stack_Addr;

            --  Temporarily map the newly allocated stack into the current
            --  address space.
            Result := Cxos.Memory.Paging.Temporarily_Map_Page (Stack_Top_Addr,
              Stack_Top_Virt_Addr);
            if Result /= Success then
               return Result;
            end if;

         end Allocate_Stack_Memory;

      --  Initialise the kernel stack.
      --  Sets the initial stack EIP.
      Initialise_Kernel_Stack :
         declare
            --  Stack frame type.
            type Stack_Frame is
              array (Natural range 1 .. 1024) of System.Address;

            New_Kernel_Stack : Stack_Frame
            with Import,
              Address => Stack_Top_Virt_Addr;
         begin
            --  Set the top of the stack frame to the initial EIP.
            New_Kernel_Stack (1023) := Initial_EIP;
         end Initialise_Kernel_Stack;

      --  Free the temporarily mapped structure.
      Result := Cxos.Memory.Paging.
        Free_Temporary_Page_Mapping (Stack_Top_Virt_Addr);
      if Result /= Success then
         return Result;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Create_New_Kernel_Stack;

   ----------------------------------------------------------------------------
   --  Mark_Kernel_Memory
   --
   --  Implementation Notes:
   --    - Marks the kernel's physical memory as being used.
   ----------------------------------------------------------------------------
   function Mark_Kernel_Memory return Process_Result is
      use Cxos.Memory.Map;

      --  The length of the kernel code segment in bytes.
      Kernel_Length    : Unsigned_32 := 0;

      --  The result of the frame status set process.
      Result : Process_Result := Success;

      --  The start address of the kernel code.
      Kernel_Start     : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_Name => "kernel_start";
      --  The end address of the kernel code.
      Kernel_End       : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_Name => "kernel_end";
      --  The physical starting address of the kernel.
      Kernel_Physical_Start : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_name => "KERNEL_PHYS_START";
   begin
      Kernel_Length   := Unsigned_32 (
        To_Integer (Kernel_End'Address) -
        To_Integer (Kernel_Start'Address));

      Result := Cxos.Memory.Map.Mark_Memory_Range (
        Kernel_Physical_Start'Address, Kernel_Length, Allocated);
      if Result /= Success then
         Cxos.Serial.Put_String (
           "Error marking kernel code segment" & ASCII.LF);

         return Unhandled_Exception;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Mark_Kernel_Memory;

end Cxos.Memory;
