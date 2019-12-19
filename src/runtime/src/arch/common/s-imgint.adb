------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ I N T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body System.Img_Int is
   -------------------
   -- Image_Integer --
   -------------------

   procedure Image_Integer
     (V : Integer;
      S : in out String;
      P : out Natural)
   is
      pragma Assert (S'First = 1);

   begin
      if V >= 0 then
         S (1) := ' ';
         P := 1;
      else
         P := 0;
      end if;

      Set_Image_Integer (V, S, P);
   exception
      when Constraint_Error =>
         return;
   end Image_Integer;

   ----------------------------------------------------------------------------
   --  Set_Image_Integer
   --
   --  Implementation Notes:
   --    - Refer to: http://www.nihamkin.com/2016/11/25/
   --      writing-linux-modules-in-ada-part-3/
   ----------------------------------------------------------------------------
   procedure Set_Image_Integer (
     V :        Integer;
     S : in out String;
     P : in out Natural
   ) is
      --  The number of digits in the resulting string.
      Digit_Count : Natural := 0;
   begin
      Get_Digit_Count :
         declare
            V2 : Integer := V;
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
   end Set_Image_Integer;

end System.Img_Int;
