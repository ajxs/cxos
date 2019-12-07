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

with System.Unsigned_Types;

-------------------------------------------------------------------------------
--  SYSTEM.IMG_UNS
--
--  Purpose:
--    This package provides an implementation of the Image attribute for
--    unsigned integer types.
-------------------------------------------------------------------------------
package System.Img_Uns is
   pragma Pure;

   ----------------------------------------------------------------------------
   --  Image_Unsigned
   --
   --  purpose:
   --    Computes Unsigned'Image (V) and stores the result in S (1 .. P) \
   --    setting the resulting value of P. The caller guarantees that S is
   --    long enough to hold the result, and that S'First is 1.
   ----------------------------------------------------------------------------
   procedure Image_Unsigned (
     V :        System.Unsigned_Types.Unsigned;
     S : in out String;
     P : out    Natural
   )
   with Inline;

   ----------------------------------------------------------------------------
   --  Set_Image_Unsigned
   --
   --  Purpose:
   --    Stores the image of V in S starting at S (P + 1), P is updated to
   --    point to the last character stored. The value stored is identical
   --    to the value of Unsigned'Image (V) except that no leading space is
   --    stored. The caller guarantees that S is long enough to hold the
   --    result. S need not have a lower bound of 1.
   ----------------------------------------------------------------------------
   procedure Set_Image_Unsigned (
      V :        System.Unsigned_Types.Unsigned;
      S : in out String;
      P : in out Natural
   );

end System.Img_Uns;
