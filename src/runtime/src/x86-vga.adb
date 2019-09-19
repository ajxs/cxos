package body x86.Vga is
   ----------------------------------------------------------------------------
   --  Clear
   --
   --  Purpose:
   --    This procedure clears the VGA text-mode buffer.
   --    Calling this function will clear the screen, filling it with the
   --    supplied background colour.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Clear (
     Bg : in Color
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
   --
   --  Purpose:
   --    This procedure prints a character to an arbitrary position within
   --    the VGA text-mode buffer.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Put_Char (
     X  : in Col;
     Y  : in Row;
     Fg : in Color;
     Bg : in Color;
     Ch : in Character
   ) is
   begin
      Vga_Output_Buffer (Y * VGA_COL_COUNT + X) := (Ch, Fg, Bg);
   exception
      when Constraint_Error =>
         null;
   end Put_Char;

   ----------------------------------------------------------------------------
   --  Put_String
   --
   --  Purpose:
   --    This procedure prints a string to an arbitrary position within
   --    the VGA text-mode buffer.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Put_String (
     X  : in Col;
     Y  : in Row;
     Fg : in Color;
     Bg : in Color;
     S  : in String
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
