------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . E X N _ I N T                        --
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

package body System.Exn_Int is

   -----------------
   -- Exn_Integer --
   -----------------

   function Exn_Integer (Left : Integer; Right : Natural) return Integer is
      pragma Suppress (Division_Check);
      pragma Suppress (Overflow_Check);

      Result : Integer;
      Factor : Integer;
      Exp    : Natural;

   begin
      Result := 1;
      Factor := Left;
      Exp    := Right;

      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2.

      --  Note: it is not worth special casing base values -1, 0, +1 since
      --  the expander does this when the base is a literal, and other cases
      --  will be extremely rare.

      if Exp /= 0 then
         loop
            if Exp rem 2 /= 0 then
               Result := Result * Factor;
            end if;

            Exp := Exp / 2;
            exit when Exp = 0;
            Factor := Factor * Factor;
         end loop;
      end if;

      return Result;
   exception
      when Constraint_Error =>
         return 0;
   end Exn_Integer;

end System.Exn_Int;
