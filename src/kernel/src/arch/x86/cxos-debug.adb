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
with Cxos.Devices.Serial;
with Interfaces; use Interfaces;

package body Cxos.Debug is
   ----------------------------------------------------------------------------
   --  Put_String
   --
   --  Implementation Notes:
   --    Prints output to COM1.
   ----------------------------------------------------------------------------
   procedure Put_String (
     Data : String
   ) is
   begin
      Cxos.Devices.Serial.Put_String (Data);
   end Put_String;

   ----------------------------------------------------------------------------
   --  Put_String_Wide
   ----------------------------------------------------------------------------
   procedure Put_String_Wide (
     Data : Wide_String
   ) is
   begin
      for C of Data loop
         --  Converts each individual wide char and prints it.
         Print_Wide_Char :
            declare
               --  An individual Char converted to its nearest ASCII match.
               Ascii_Char : Character;
            begin
               Ascii_Char :=
                 Character'Val (Unsigned_8 (Wide_Character'Pos (C)));
               Cxos.Devices.Serial.Put_String ("" & Ascii_Char);
            exception
               when Constraint_Error =>
                  Cxos.Devices.Serial.
                    Put_String ("" & Ada.Characters.Latin_1.NUL);
            end Print_Wide_Char;
      end loop;
   end Put_String_Wide;

end Cxos.Debug;
