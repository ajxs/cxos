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

with System.Machine_Code;

package body x86.Port_IO is
   ----------------------------------------------------------------------------
   --  Inb
   ----------------------------------------------------------------------------
   function Inb (
     Port : System.Address
   ) return Interfaces.Unsigned_8 is
      Data : Interfaces.Unsigned_8;
   begin
      System.Machine_Code.Asm (
        Template => "inb %w1, %0",
        Inputs => (
          System.Address'Asm_Input ("Nd", Port)
        ),
        Outputs => (
          Interfaces.Unsigned_8'Asm_Output ("=a", Data)
        ),
        Volatile => True);

      return Data;
   end Inb;

   ----------------------------------------------------------------------------
   --  Outb
   ----------------------------------------------------------------------------
   procedure Outb (
     Port : System.Address;
     Data : Interfaces.Unsigned_8
   ) is
   begin
      System.Machine_Code.Asm (
        Template => "outb %0, %w1",
        Inputs => (
          Interfaces.Unsigned_8'Asm_Input ("a", Data),
          System.Address'Asm_Input ("Nd", Port)
        ),
        Volatile => True);
   end Outb;
end x86.Port_IO;
