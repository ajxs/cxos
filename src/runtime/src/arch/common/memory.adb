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

package body Memory is
   ----------------------------------------------------------------------------
   --  Copy
   ----------------------------------------------------------------------------
   function Copy (
     Dest   : System.Address;
     Source : System.Address;
     Count  : Integer
   ) return System.Address is
      --  The source array.
      Source_Array : Byte_Array (1 .. Count)
      with Import,
        Address => Source;
      --  The destination array.
      Dest_Array   : Byte_Array (1 .. Count)
      with Import,
        Address => Dest;
   begin
      --  If there is no length, do nothing.
      if Count < 1 then
         return Dest;
      end if;

      --  If the source and destinations are equal, do nothing.
      if Dest = Source then
         return Dest;
      end if;

      --  Copy via slicing.
      Dest_Array (1 .. Count) := Source_Array (1 .. Count);

      return Dest;
   exception
      when Constraint_Error =>
         return Null_Address;
   end Copy;

   ----------------------------------------------------------------------------
   --  Move
   ----------------------------------------------------------------------------
   function Move (
     Dest   : System.Address;
     Source : System.Address;
     Count  : Integer
   ) return System.Address is
      --  The source array.
      Source_Array : Byte_Array (1 .. Count)
      with Import,
        Address => Source;
      --  The destination array.
      Dest_Array   : Byte_Array (1 .. Count)
      with Import,
        Address => Dest;
   begin
      --  If there is no length, do nothing.
      if Count < 1 then
         return Dest;
      end if;

      --  If the source and destinations are equal, do nothing.
      if Dest = Source then
         return Dest;
      end if;

      if Source < Dest then
         --  If the source is less than the destination, copy backwards to
         --  avoid any issues arising from the memory spaces overlapping.
         Copy_Downwards :
            declare
               --  Loop counter.
               I : Integer;
            begin
               I := Count;

               loop
                  Dest_Array (I) := Source_Array (I);

                  I := I - 1;
                  exit when I = 0;
               end loop;

               return Dest;
            exception
               when Constraint_Error =>
                  return Null_Address;
            end Copy_Downwards;
      end if;

      for I in 1 .. Count loop
         Dest_Array (I) := Source_Array (I);
      end loop;

      return Dest;
   end Move;
end Memory;
