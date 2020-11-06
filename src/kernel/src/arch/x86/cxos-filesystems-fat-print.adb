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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Cxos.Debug;
with Cxos.Error_Handling;

package body Cxos.Filesystems.FAT.Print is
   package Chars renames Ada.Characters.Latin_1;
   procedure Debug_Print (Data : String) renames Cxos.Debug.Put_String;
   --  Error logging function shorthand.
   procedure Log_Error (Message : String)
     renames Cxos.Error_Handling.Log_Kernel_Error;

   ----------------------------------------------------------------------------
   --  Print_Filesystem_Info
   ----------------------------------------------------------------------------
   procedure Print_Filesystem_Info (Boot_Sector : Boot_Sector_T) is
      --  The extended BIOS parameter block, if the filesystem is FAT12/16.
      EBPB : Extended_BIOS_Parameter_Block
      with Import,
        Convention => Ada,
        Address    => Boot_Sector.BPB_Buffer'Address;

      --  The type of this FAT filesystem.
      Filesystem_Type : FAT_Type_T := FAT12;
      --  The status of internal operations.
      Status          : Program_Status;
   begin
      Get_Filesystem_Type (Boot_Sector, Filesystem_Type, Status);
      if Status /= Success then
         Log_Error ("Error getting FAT device filesystem type" & Chars.LF);
         return;
      end if;

      Debug_Print ("FAT Filesystem:" & Chars.LF);

      Debug_Print ("  FAT type:              ");
      case Filesystem_Type is
         when FAT12 =>
            Debug_Print ("FAT12");
         when FAT16 =>
            Debug_Print ("FAT16");
         when FAT32 =>
            Debug_Print ("FAT32");
         when ExFAT =>
            Debug_Print ("ExFAT");
      end case;
      Debug_Print ("" & Chars.LF);

      Debug_Print ("  Boot Jump Bytes:       ");
      for I in Natural range 1 .. 3 loop
         Debug_Print ("" & Boot_Sector.Boot_Jump (I)'Image);
      end loop;
      Debug_Print ("" & Chars.LF);

      Debug_Print ("  OEM name:              ");
      for I in Natural range 1 .. 8 loop
         Debug_Print ("" & Boot_Sector.OEM_Name (I));
      end loop;
      Debug_Print ("" & Chars.LF);

      Debug_Print ("  Bytes_Per_Sector:      " &
        EBPB.BPB.Bytes_Per_Sector'Image & Chars.LF);
      Debug_Print ("  Sectors_Per_Cluster:   " &
        EBPB.BPB.Sectors_Per_Cluster'Image & Chars.LF);
      Debug_Print ("  Reserved_Sector_Count: " &
        EBPB.BPB.Reserved_Sector_Count'Image & Chars.LF);
      Debug_Print ("  Table_Count:           " &
        EBPB.BPB.Table_Count'Image & Chars.LF);
      Debug_Print ("  Root_Entry_Count:      " &
        EBPB.BPB.Root_Entry_Count'Image & Chars.LF);
      Debug_Print ("  Total_Sector_Count:    " &
        EBPB.BPB.Total_Sector_Count'Image & Chars.LF);
      Debug_Print ("  Table_Size:            " &
        EBPB.BPB.Table_Size'Image & Chars.LF);
      Debug_Print ("  Media_Type:            " &
        EBPB.BPB.Media_Type'Image & Chars.LF);
      Debug_Print ("  Sectors_Per_Track:     " &
        EBPB.BPB.Sectors_Per_Track'Image & Chars.LF);
      Debug_Print ("  Head_Side_Count:       " &
        EBPB.BPB.Head_Side_Count'Image & Chars.LF);
      Debug_Print ("  Hidden_Sector_Count:   " &
        EBPB.BPB.Hidden_Sector_Count'Image & Chars.LF);
      Debug_Print ("  Large_Sector_Count:    " &
        EBPB.BPB.Large_Sector_Count'Image & Chars.LF);

      Debug_Print ("------------------------" & Chars.LF);
   exception
      when Constraint_Error =>
         return;
   end Print_Filesystem_Info;

end Cxos.Filesystems.FAT.Print;
