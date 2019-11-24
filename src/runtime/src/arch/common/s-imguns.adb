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

with System.Unsigned_Types; use System.Unsigned_Types;

package body System.Img_Uns is
   ----------------------------------------------------------------------------
   --  Image_Unsigned
   ----------------------------------------------------------------------------
   procedure Image_Unsigned (
     V :        System.Unsigned_Types.Unsigned;
     S : in out String;
     P : out    Natural
   ) is
      pragma Assert (S'First = 1);
   begin
      S (1) := ' ';
      P := 1;
      Set_Image_Unsigned (V, S, P);
   exception
      when Constraint_Error =>
         return;
   end Image_Unsigned;

   ----------------------------------------------------------------------------
   --  Set_Image_Unsigned
   --
   --  Implementation Notes:
   --    - Refer to: http://www.nihamkin.com/2016/11/25/
   --      writing-linux-modules-in-ada-part-3/
   ----------------------------------------------------------------------------
   procedure Set_Image_Unsigned (
     V :        Unsigned;
     S : in out String;
     P : in out Natural
   ) is
      --  The number of digits in the resulting string.
      Digit_Count : Natural := 0;
   begin
      Get_Digit_Count :
         declare
            V2 : Unsigned := V;
         begin
            while V2 /= 0 loop
               Digit_Count := Digit_Count + 1;
               V2 := V2 / 10;
            end loop;
         exception
            when Constraint_Error =>
               Digit_Count := 0;
         end Get_Digit_Count;

      Write_To_String :
         begin
            if Digit_Count = 0 then
               P := P + 1;
               S (P) := '0';
            else
               for I in reverse 0 .. (Digit_Count - 1) loop
                  P := P + 1;
                  S (P) := Character'Val (48 + (V / 10 ** I) rem 10);
               end loop;
            end if;
         exception
            when Constraint_Error =>
               return;
         end Write_To_String;
   end Set_Image_Unsigned;

end System.Img_Uns;
