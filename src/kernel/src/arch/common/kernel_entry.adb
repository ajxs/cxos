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

with Cxos;
with Cxos.Serial;
with Interfaces;
with Multiboot;

-------------------------------------------------------------------------------
--  Kernel_Entry
--
--  Purpose:
--    The Kernel_Entry procedure is the main entry point for the kernel.
--    This procedure is called from the boot code contained within the
--    kernel's Ada runtime, and serves as the transition between the boot
--    code and the kernel functionality.
--
-------------------------------------------------------------------------------
procedure Kernel_Entry is
   use Cxos;
   use Interfaces;
   use Multiboot;

   --  The Multiboot magic number. This is setup during boot.
   Magic_Number : Multiboot_Magic_Number
   with Import,
     Convention    => Assembler,
     External_Name => "multiboot_magic";

   --  The process result of initialising the kernel.
   Init_Result  : Cxos.Kernel_Process_Result;
begin
   --  Check whether we were booted by a Multiboot compatible bootloader.
   if Magic_Number = VALID_MAGIC_NUMBER then
      Cxos.Serial.Put_String (
        "Detected valid Multiboot magic number" & ASCII.LF);
   else
      Cxos.Serial.Put_String (
        "Unable to detect valid Multiboot magic number" & ASCII.LF);
   end if;

   Init_Result := Cxos.Initialise_Kernel;
   if Init_Result /= Success then
      Cxos.Serial.Put_String ("Kernel initialisation failed" & ASCII.LF);
   end if;

   --  Call the main kernel function.
   Cxos.Main;
exception
   when Constraint_Error =>
      return;
end Kernel_Entry;
