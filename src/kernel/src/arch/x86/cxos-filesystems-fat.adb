with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Cxos.Debug;

package body Cxos.Filesystems.FAT is
   package Chars renames Ada.Characters.Latin_1;
   procedure Debug_Print (Data : String) renames Cxos.Debug.Put_String;

   ----------------------------------------------------------------------------
   --  Get_Filesystem_Type
   ----------------------------------------------------------------------------
   procedure Get_Filesystem_Type (
     Boot_Sec :     Boot_Sector;
     FAT_Type : out FAT_Type_T;
     Status   : out Program_Status
   ) is
      --  The extended BIOS parameter block, if the filesystem is FAT12/16.
      EBPB : Extended_BIOS_Parameter_Block
      with Import,
        Convention => Ada,
        Address    => Boot_Sec.BPB_Buffer'Address;

      --  The extended FAT32 BIOS parameter block,
      --  if the filesystem is FAT32.
      FAT32_EBPB : FAT32_Extended_BIOS_Parameter_Block
      with Import,
        Convention => Ada,
        Address    => Boot_Sec.BPB_Buffer'Address;

      --  The size of the FAT table.
      FAT_Size       : Unsigned_32 := 0;
      --  The total number of sectors.
      Total_Sectors  : Unsigned_32 := 0;
      --  The number of data sectors.
      Data_Sectors   : Unsigned_32 := 0;
      --  Total clusters.
      Total_Clusters : Unsigned_32 := 0;
   begin
      Status := Success;

      if EBPB.BPB.Table_Size = 0 then
         FAT_Size := FAT32_EBPB.Table_Size;
      else
         FAT_Size := Unsigned_32 (EBPB.BPB.Table_Size);
      end if;

      if EBPB.BPB.Total_Sector_Count = 0 then
         Total_Sectors := FAT32_EBPB.BPB.Large_Sector_Count;
      else
         Total_Sectors := Unsigned_32 (EBPB.BPB.Total_Sector_Count);
      end if;

      Data_Sectors := Total_Sectors -
        (Unsigned_32 (EBPB.BPB.Reserved_Sector_Count) +
        (Unsigned_32 (EBPB.BPB.Table_Count) * FAT_Size));

      Total_Clusters := Data_Sectors /
        Unsigned_32 (EBPB.BPB.Sectors_Per_Cluster);

      if Total_Clusters < 4085 then
         FAT_Type := FAT12;
      elsif Total_Clusters < 65525 then
         FAT_Type := FAT16;
      elsif Total_Clusters < 268435445 then
         FAT_Type := FAT32;
      else
         FAT_Type := ExFAT;
      end if;
   exception
      when Constraint_Error =>
         Status := Failure;
   end Get_Filesystem_Type;

   ----------------------------------------------------------------------------
   --  Parse_Directory
   ----------------------------------------------------------------------------
   procedure Parse_Directory (
     Directory_Buffer_Addr : System.Address;
     Directory_Size        : Natural;
     Status                : out Program_Status
   ) is
   begin
      Load_Index :
      declare
         Index : Directory_Index (1 .. Directory_Size)
         with Import,
           Convention => Ada,
           Address    => Directory_Buffer_Addr;
      begin
         for I in 1 .. Directory_Size loop
            for K in Natural range 0 .. 10 loop
               Debug_Print ("" & Index (I).File_Name (K));
            end loop;

            Debug_Print ("" & Chars.LF);
         end loop;

      end Load_Index;

      Status := Success;
   exception
      when Constraint_Error =>
         null;
   end Parse_Directory;

   ----------------------------------------------------------------------------
   --  Print_Filesystem_Info
   ----------------------------------------------------------------------------
   procedure Print_Filesystem_Info (Boot_Sec : Boot_Sector) is
      use Cxos.Debug;

      --  The extended BIOS parameter block, if the filesystem is FAT12/16.
      EBPB : Extended_BIOS_Parameter_Block
      with Import,
        Convention => Ada,
        Address    => Boot_Sec.BPB_Buffer'Address;

      --  The type of this FAT filesystem.
      Filesystem_Type : FAT_Type_T := FAT12;
      --  The status of internal operations.
      Status          : Program_Status;
   begin
      Get_Filesystem_Type (Boot_Sec, Filesystem_Type, Status);
      if Status /= Success then
         Put_String ("Error getting FAT device filesystem type" & Chars.LF);
         return;
      end if;

      Put_String ("FAT Filesystem:" & Chars.LF);

      Put_String ("  FAT type:              ");
      case Filesystem_Type is
         when FAT12 =>
            Put_String ("FAT12");
         when FAT16 =>
            Put_String ("FAT16");
         when FAT32 =>
            Put_String ("FAT32");
         when ExFAT =>
            Put_String ("ExFAT");
      end case;
      Put_String ("" & Chars.LF);

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
        EBPB.BPB.Bytes_Per_Sector'Image & Chars.LF);
      Put_String ("  Sectors_Per_Cluster:   " &
        EBPB.BPB.Sectors_Per_Cluster'Image & Chars.LF);
      Put_String ("  Reserved_Sector_Count: " &
        EBPB.BPB.Reserved_Sector_Count'Image & Chars.LF);
      Put_String ("  Table_Count:           " &
        EBPB.BPB.Table_Count'Image & Chars.LF);
      Put_String ("  Root_Entry_Count:      " &
        EBPB.BPB.Root_Entry_Count'Image & Chars.LF);
      Put_String ("  Total_Sector_Count:    " &
        EBPB.BPB.Total_Sector_Count'Image & Chars.LF);
      Put_String ("  Table_Size:            " &
        EBPB.BPB.Table_Size'Image & Chars.LF);
      Put_String ("  Media_Type:            " &
        EBPB.BPB.Media_Type'Image & Chars.LF);
      Put_String ("  Sectors_Per_Track:     " &
        EBPB.BPB.Sectors_Per_Track'Image & Chars.LF);
      Put_String ("  Head_Side_Count:       " &
        EBPB.BPB.Head_Side_Count'Image & Chars.LF);
      Put_String ("  Hidden_Sector_Count:   " &
        EBPB.BPB.Hidden_Sector_Count'Image & Chars.LF);
      Put_String ("  Large_Sector_Count:    " &
        EBPB.BPB.Large_Sector_Count'Image & Chars.LF);

      Put_String ("------------------------" & Chars.LF);
   exception
      when Constraint_Error =>
         return;
   end Print_Filesystem_Info;

end Cxos.Filesystems.FAT;
