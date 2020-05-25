with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Cxos.Debug;

package body Cxos.Filesystems.FAT is
   package Chars renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Filesystem_Type (Boot_Sec : Boot_Sector) return FAT_Type is
      Total_Sectors : Unsigned_32 := 0;
   begin
      if Boot_Sec.BPB.Total_Sector_Count = 0 then
         Total_Sectors := Boot_Sec.BPB.Large_Sector_Count;
      else
         Total_Sectors := Unsigned_32 (Boot_Sec.BPB.Total_Sector_Count);
      end if;

      if Total_Sectors < 10 then
         return FAT12;
      end if;

      return FAT32;
   end Get_Filesystem_Type;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Print_Filesystem_Info (Boot_Sec : Boot_Sector) is
      use Cxos.Debug;
   begin
      Put_String ("FAT Filesystem:" & Chars.LF)
      ;
      Put_String ("  Boot Jump Bytes:       ");
      for I in Natural range 1 .. 3 loop
         Put_String ("" & Boot_Sec.Boot_Jump (I)'Image);
      end loop;
      Put_String ("" & Chars.LF);

      Put_String ("  OEM name:              ");
      for I in Natural range 1 .. 8 loop
         Put_String ("" & Boot_Sec.OEM_Name (I));
      end loop;
      Put_String ("" & Chars.LF);

      Put_String ("  Bytes_Per_Sector:      " &
        Boot_Sec.BPB.Bytes_Per_Sector'Image & Chars.LF);
      Put_String ("  Sectors_Per_Cluster:   " &
        Boot_Sec.BPB.Sectors_Per_Cluster'Image & Chars.LF);
      Put_String ("  Reserved_Sector_Count: " &
        Boot_Sec.BPB.Reserved_Sector_Count'Image & Chars.LF);
      Put_String ("  Table_Count:           " &
        Boot_Sec.BPB.Table_Count'Image & Chars.LF);
      Put_String ("  Root_Entry_Count:      " &
        Boot_Sec.BPB.Root_Entry_Count'Image & Chars.LF);
      Put_String ("  Total_Sector_Count:    " &
        Boot_Sec.BPB.Total_Sector_Count'Image & Chars.LF);
      Put_String ("  Table_Size:            " &
        Boot_Sec.BPB.Table_Size'Image & Chars.LF);
      Put_String ("  Media_Type:            " &
        Boot_Sec.BPB.Media_Type'Image & Chars.LF);
      Put_String ("  Sectors_Per_Track:     " &
        Boot_Sec.BPB.Sectors_Per_Track'Image & Chars.LF);
      Put_String ("  Head_Side_Count:       " &
        Boot_Sec.BPB.Head_Side_Count'Image & Chars.LF);
      Put_String ("  Hidden_Sector_Count:   " &
        Boot_Sec.BPB.Hidden_Sector_Count'Image & Chars.LF);
      Put_String ("  Large_Sector_Count:    " &
        Boot_Sec.BPB.Large_Sector_Count'Image & Chars.LF);

      Put_String ("------------------------" & Chars.LF);
   end Print_Filesystem_Info;

end Cxos.Filesystems.FAT;
