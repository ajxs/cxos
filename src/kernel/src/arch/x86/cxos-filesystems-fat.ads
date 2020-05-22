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
   type FAT_Type is (
     FAT12,
     FAT16,
     FAT32
   );

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type Boot_Jump_Bytes is
     array (1 .. 3) of Unsigned_8;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type OEM_Name_Type is
     array (1 .. 8) of Character;

   ----------------------------------------------------------------------------4Y
   ----------------------------------------------------------------------------
   type BIOS_Parameter_Block is
      record
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
   with Size => 200;
   for BIOS_Parameter_Block use
      record
         Bytes_Per_Sector      at 0 range 0   .. 15;
         Sectors_Per_Cluster   at 0 range 16  .. 23;
         Reserved_Sector_Count at 0 range 24  .. 39;
         Table_Count           at 0 range 40  .. 47;
         Root_Entry_Count      at 0 range 48  .. 63;
         Total_Sector_Count    at 0 range 64  .. 79;
         Media_Type            at 0 range 80  .. 87;
         Table_Size            at 0 range 88  .. 103;
         Sectors_Per_Track     at 0 range 104 .. 119;
         Head_Side_Count       at 0 range 120 .. 135;
         Hidden_Sector_Count   at 0 range 136 .. 167;
         Large_Sector_Count    at 0 range 168 .. 199;
      end record;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type Partition_Volume_Label_String is
     array (1 .. 11) of Character;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type File_System_Type_String is
     array (1 .. 8) of Character;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type Extended_BIOS_Parameter_Block is
      record
         Physical_Drive_Number   : Unsigned_8;
         Reserved                : Unsigned_8;
         Extended_Boot_Signature : Unsigned_8;
         Volume_Id               : Unsigned_32;
         Partition_Volume_Label  : Partition_Volume_Label_String;
         File_System_Type        : File_System_Type_String;
      end record
   with Size => 208;
   for Extended_BIOS_Parameter_Block use
      record
         Physical_Drive_Number   at 0 range 0   .. 7;
         Reserved                at 0 range 8   .. 15;
         Extended_Boot_Signature at 0 range 16  .. 23;
         Volume_Id               at 0 range 24  .. 55;
         Partition_Volume_Label  at 0 range 56  .. 143;
         File_System_Type        at 0 range 144  .. 207;
      end record;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type Boot_Sector is
      record
         Boot_Jump : Boot_Jump_Bytes;
         OEM_Name  : OEM_Name_Type;
         BPB       : BIOS_Parameter_Block;
      end record;
   for Boot_Sector use
      record
         Boot_Jump at 0 range 0  .. 23;
         OEM_Name  at 0 range 24 .. 87;
         BPB       at 0 range 88 .. 287;
      end record;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT12_Table_Entry is mod 2 ** 12
   with Size => 12;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT12_Table is
     array (Natural range <>) of FAT12_Table_Entry
     with Pack;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT16_Table_Entry is new Unsigned_16;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT16_Table is
     array (Natural range <>) of FAT16_Table_Entry;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT32_Table_Entry is new Unsigned_32;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT32_Table is
     array (Natural range <>) of aliased FAT32_Table_Entry;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Print_Filesystem_Info (Boot_Sec : Boot_Sector);

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Filesystem_Type (Boot_Sec : Boot_Sector) return FAT_Type;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_FAT_Size (Boot_Sec : Boot_Sector) return Unsigned_32;

end Cxos.Filesystems.FAT;
