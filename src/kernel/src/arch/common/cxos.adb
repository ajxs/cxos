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
with Cxos.Tasking;

package body Cxos is
   package Chars renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------------
   --  Main
   ----------------------------------------------------------------------------
   procedure Main is
   begin
      --  Print the ASCII splash screen.
      Print_Splash;

      loop
         Cxos.Tasking.Idle;
      end loop;
   end Main;

   ----------------------------------------------------------------------------
   --  Print_Splash
   ----------------------------------------------------------------------------
   procedure Print_Splash is
      Line_1 : constant String := "  /$$$$$$  /$$   /$$  /$$$$$$   /$$$$$$ ";
      Line_2 : constant String := " /$$__  $$| $$  / $$ /$$__  $$ /$$__  $$";
      Line_3 : constant String := "| $$  \__/|  $$/ $$/| $$  \ $$| $$  \__/";
      Line_4 : constant String := "| $$       \  $$$$/ | $$  | $$|  $$$$$$ ";
      Line_5 : constant String := "| $$        >$$  $$ | $$  | $$ \____  $$";
      Line_6 : constant String := "| $$    $$ /$$/\  $$| $$  | $$ /$$  \ $$";
      Line_7 : constant String := "|  $$$$$$/| $$  \ $$|  $$$$$$/|  $$$$$$/";
      Line_8 : constant String := " \______/ |__/  |__/ \______/  \______/ ";
   begin
      Print_Splash_to_Serial :
         begin
            Cxos.Debug.Put_String ("" & Chars.LF);
            Cxos.Debug.Put_String (Line_1 & Chars.LF);
            Cxos.Debug.Put_String (Line_2 & Chars.LF);
            Cxos.Debug.Put_String (Line_3 & Chars.LF);
            Cxos.Debug.Put_String (Line_4 & Chars.LF);
            Cxos.Debug.Put_String (Line_5 & Chars.LF);
            Cxos.Debug.Put_String (Line_6 & Chars.LF);
            Cxos.Debug.Put_String (Line_7 & Chars.LF);
            Cxos.Debug.Put_String (Line_8 & Chars.LF);
            Cxos.Debug.Put_String ("" & Chars.LF);
         end Print_Splash_to_Serial;
   end Print_Splash;
end Cxos;
