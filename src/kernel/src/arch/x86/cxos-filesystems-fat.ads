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

with Interfaces; use Interfaces;

-------------------------------------------------------------------------------
--  CXOS.FILESYSTEMS.FAT
--
--  Purpose:
--    This package contains definitons and functionality for working with
--    FAT filesystems.
-------------------------------------------------------------------------------
package Cxos.Filesystems.FAT is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type Boot_Jump_Bytes is
     array (1 .. 3) of Unsigned_8;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type OEM_Name_Type is
     array (1 .. 8) of Character;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type BIOS_Parameter_Block is
      record
         Boot_Jump             : Boot_Jump_Bytes;
         OEM_Name              : OEM_Name_Type;
         Bytes_Per_Sector      : Unsigned_16;
         Sectors_Per_Cluster   : Unsigned_8;
         Reserved_Sector_Count : Unsigned_16;
         Table_Count           : Unsigned_8;
         Root_Entry_Count      : Unsigned_16;
         Total_Sector_Count    : Unsigned_16;
         Media_Type            : Unsigned_8;
         Table_Size            : Unsigned_16;
         Sectors_Per_Track     : Unsigned_16;
         Head_Side_Count       : Unsigned_16;
         Hidden_Sector_Count   : Unsigned_32;
         Large_Sector_Count    : Unsigned_32;
      end record
   with Size => 288;
   for BIOS_Parameter_Block use
      record
         Boot_Jump             at 0 range 0   .. 23;
         OEM_Name              at 0 range 24  .. 87;
         Bytes_Per_Sector      at 0 range 88  .. 103;
         Sectors_Per_Cluster   at 0 range 104 .. 111;
         Reserved_Sector_Count at 0 range 112 .. 127;
         Table_Count           at 0 range 128 .. 135;
         Root_Entry_Count      at 0 range 136 .. 151;
         Total_Sector_Count    at 0 range 152 .. 167;
         Media_Type            at 0 range 168 .. 175;
         Table_Size            at 0 range 176 .. 191;
         Sectors_Per_Track     at 0 range 192 .. 207;
         Head_Side_Count       at 0 range 208 .. 223;
         Hidden_Sector_Count   at 0 range 224 .. 255;
         Large_Sector_Count    at 0 range 256 .. 287;
      end record;

end Cxos.Filesystems.FAT;
