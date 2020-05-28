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
   --  Program status type.
   --  Used to track the results of functions.
   ----------------------------------------------------------------------------
   type Program_Status is (
     Failure,
     Success
   );

   ----------------------------------------------------------------------------
   --  The type of FAT filesystem.
   ----------------------------------------------------------------------------
   type FAT_Type is (
     FAT12,
     FAT16,
     FAT32,
     ExFAT
   );

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
         BPB                     : BIOS_Parameter_Block;
         Physical_Drive_Number   : Unsigned_8;
         Reserved                : Unsigned_8;
         Extended_Boot_Signature : Unsigned_8;
         Volume_Id               : Unsigned_32;
         Partition_Volume_Label  : Partition_Volume_Label_String;
         File_System_Type        : File_System_Type_String;
      end record
   with Size => 408;
   for Extended_BIOS_Parameter_Block use
      record
         BPB                     at 0 range 0   .. 199;
         Physical_Drive_Number   at 0 range 200 .. 207;
         Reserved                at 0 range 208 .. 215;
         Extended_Boot_Signature at 0 range 216 .. 223;
         Volume_Id               at 0 range 224 .. 255;
         Partition_Volume_Label  at 0 range 256 .. 343;
         File_System_Type        at 0 range 344 .. 407;
      end record;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT32_Volume_Label is array (0 .. 10) of Character;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT32_Type_Label is array (0 .. 7) of Character;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT32_Reserved_Buffer is array (0 .. 11) of Character;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type FAT32_Extended_BIOS_Parameter_Block is
      record
         BPB              : BIOS_Parameter_Block;
         Table_Size       : Unsigned_32;
         Drive_Desc       : Unsigned_16;
         Version          : Unsigned_16;
         Root_Cluster     : Unsigned_32;
         Info_Sector      : Unsigned_16;
         Backup_BS_Sector : Unsigned_16;
         Reserved         : FAT32_Reserved_Buffer;
         Drive_Number     : Unsigned_8;
         Reserved_1       : Unsigned_8;
         Boot_Signature   : Unsigned_8;
         Volume_ID        : Unsigned_8;
         Volume_Label     : FAT32_Volume_Label;
         Type_Label       : FAT32_Type_Label;
      end record
   with Size => 610;
   for FAT32_Extended_BIOS_Parameter_Block use
      record
         BPB              at 0 range 0   .. 199;
         Table_Size       at 0 range 200 .. 231;
         Drive_Desc       at 0 range 232 .. 247;
         Version          at 0 range 248 .. 265;
         Root_Cluster     at 0 range 266 .. 297;
         Info_Sector      at 0 range 298 .. 313;
         Backup_BS_Sector at 0 range 314 .. 329;
         Reserved         at 0 range 330 .. 425;
         Drive_Number     at 0 range 426 .. 433;
         Reserved_1       at 0 range 434 .. 441;
         Boot_Signature   at 0 range 442 .. 449;
         Volume_ID        at 0 range 450 .. 457;
         Volume_Label     at 0 range 458 .. 545;
         Type_Label       at 0 range 546 .. 609;
      end record;

   ----------------------------------------------------------------------------
   --  This buffer type represents the space reserved in the FAT boot sector
   --  for the BIOS Parameter Block.
   --  This reserved buffer type is used since we will not know ahead of time
   --  what FAT type we are dealing with, and will need to be able to convert
   --  between the different BPB types.
   ----------------------------------------------------------------------------
   type Reserved_BIOS_Parameter_Block_Buffer is
     array (Natural range 0 .. 89) of Unsigned_8;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   type Boot_Sector is
      record
         Boot_Jump  : Boot_Jump_Bytes;
         OEM_Name   : OEM_Name_Type;
         BPB_Buffer : Reserved_BIOS_Parameter_Block_Buffer;
      end record;
   for Boot_Sector use
      record
         Boot_Jump  at 0 range 0  .. 23;
         OEM_Name   at 0 range 24 .. 87;
         BPB_Buffer at 0 range 88 .. 807;
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

end Cxos.Filesystems.FAT;
