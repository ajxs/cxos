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

with System.Storage_Elements;

-------------------------------------------------------------------------------
--  X86.VGA
--
--  Purpose:
--    This package contains a basic VGA text-mode driver.
--    The procedures and type definitions contained within this module can be
--    used for printing text to the system VGA text-mode buffer.
-------------------------------------------------------------------------------
package x86.Vga is
   pragma Preelaborate (x86.Vga);

   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  The predefined VGA color codes.
   ----------------------------------------------------------------------------
   type Color is (
     Black,
     Blue,
     Green,
     Cyan,
     Red,
     Magenta,
     Brown,
     Light_Grey,
     Dark_Grey,
     Light_Blue,
     Light_Green,
     Light_Cyan,
     Light_Red,
     Light_Magenta,
     Yellow,
     White
   )
   with Size => 4;
   for Color use (
     Black         => 0,
     Blue          => 1,
     Green         => 2,
     Cyan          => 3,
     Red           => 4,
     Magenta       => 5,
     Brown         => 6,
     Light_Grey    => 7,
     Dark_Grey     => 8,
     Light_Blue    => 9,
     Light_Green   => 10,
     Light_Cyan    => 11,
     Light_Red     => 12,
     Light_Magenta => 13,
     Yellow        => 14,
     White         => 15
   );

   VGA_COL_COUNT : constant := 80;
   VGA_ROW_COUNT : constant := 24;

   subtype Col is Natural range 0 .. VGA_COL_COUNT - 1;
   subtype Row is Natural range 0 .. VGA_ROW_COUNT - 1;

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
     X  : Col;
     Y  : Row;
     Fg : Color;
     Bg : Color;
     S  : String
   );

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
     Bg : Color
   );

private
   ----------------------------------------------------------------------------
   --  Represents the encoding of an individual character entry in the
   --  VGA screen buffer.
   ----------------------------------------------------------------------------
   type Vga_Buffer_Char is
      record
         Char       : Character;
         Foreground : Color;
         Background : Color;
      end record
   with Size => 16;
   for Vga_Buffer_Char use
      record
         Char       at 0 range 0 .. 7;
         Foreground at 1 range 0 .. 3;
         Background at 1 range 4 .. 7;
      end record;

   type Vga_Buffer is array (Natural range <>) of Vga_Buffer_Char;

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
     X  : Col;
     Y  : Row;
     Fg : Color;
     Bg : Color;
     Ch : Character
   );

   ----------------------------------------------------------------------------
   --  The actual VGA screen buffer memory.
   ----------------------------------------------------------------------------
   Vga_Output_Buffer : Vga_Buffer (0 .. (VGA_COL_COUNT * VGA_ROW_COUNT) - 1)
   with Import,
     Convention => Ada,
     Address    => To_Address (16#C03F_E000#),
     Volatile;

end x86.Vga;
