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

with System.Storage_Elements; use System.Storage_Elements;
with x86.Vga; use x86.Vga;

-------------------------------------------------------------------------------
--  CXOS.DEVICES.GRAPHICS.VGA
--
--  Purpose:
--    Package for interfacing with VGA hardware.
-------------------------------------------------------------------------------
package Cxos.Devices.Graphics.Vga is
   pragma Preelaborate;

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
   --  The address of the VGA memory buffer.
   ----------------------------------------------------------------------------
   VGA_MEMORY_ADDRESS : constant Integer_Address := 16#FF400000#;

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
     Address    => To_Address (VGA_MEMORY_ADDRESS),
     Volatile;

end Cxos.Devices.Graphics.Vga;
