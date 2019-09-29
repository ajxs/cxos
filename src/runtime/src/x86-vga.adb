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
