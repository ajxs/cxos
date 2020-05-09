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

with Ada.Characters.Latin_1;
with Cxos.Debug;
with Cxos.Memory.Map;
with Interfaces; use Interfaces;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

package body Cxos.Memory is
   package Chars renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------------
   --  Current_Page_Dir_Ptr
   ----------------------------------------------------------------------------
   function Current_Page_Dir_Ptr return System.Address is
      CR3 : System.Address;
   begin
      System.Machine_Code.Asm (
        Template => "movl %%cr3, %0",
        Outputs => (
          System.Address'Asm_Output ("=a", CR3)
        ),
        Volatile => True);

      --  Return the final entry in the currently loaded page directory.
      return CR3;
   end Current_Page_Dir_Ptr;

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
         Cxos.Debug.Put_String (
           "Error marking kernel code segment" & Chars.LF);

         return Unhandled_Exception;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Mark_Kernel_Memory;

end Cxos.Memory;
