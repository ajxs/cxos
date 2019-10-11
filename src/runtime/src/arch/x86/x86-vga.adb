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

package body x86.Vga is
   ----------------------------------------------------------------------------
   --  Clear
   ----------------------------------------------------------------------------
   procedure Clear (
     Bg : Color
   ) is
   begin
      for X in Col'Range loop
         for Y in Row'Range loop
            Put_Char (X, Y, Bg, Bg, ' ');
         end loop;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Clear;

   ----------------------------------------------------------------------------
   --  Put_Char
   ----------------------------------------------------------------------------
   procedure Put_Char (
     X  : Col;
     Y  : Row;
     Fg : Color;
     Bg : Color;
     Ch : Character
   ) is
   begin
      Vga_Output_Buffer (Y * VGA_COL_COUNT + X) := (Ch, Fg, Bg);
   exception
      when Constraint_Error =>
         null;
   end Put_Char;

   ----------------------------------------------------------------------------
   --  Put_String
   ----------------------------------------------------------------------------
   procedure Put_String (
     X  : Col;
     Y  : Row;
     Fg : Color;
     Bg : Color;
     S  : String
   ) is
      C : Natural := 0;
   begin
      for I in S'Range loop
         Put_Char (X + C, Y, Fg, Bg, S (I));
         C := C + 1;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Put_String;
end x86.Vga;
