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
      Source_Array : Byte_Array (0 .. Count)
      with Import,
        Convention => C,
        Address    => Source;

      Dest_Array   : Byte_Array (0 .. Count)
      with Import,
        Convention => C,
        Address    => Dest;
   begin
      --  If there is no length, do nothing.
      if Count < 1 then
         return Dest;
      end if;

      for Idx in Source_Array'Range loop
         Dest_Array (Idx) := Source_Array (Idx);
      end loop;

      return Dest;
   end Copy;

end Memory;
