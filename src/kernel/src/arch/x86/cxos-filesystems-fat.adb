with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Cxos.Debug;
with Cxos.VFS;

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

         File_Idx      : Natural := 1;
         Directory_Files : array (1 .. 8) of Cxos.VFS.File_T;
      begin
         for I in 1 .. Directory_Size loop
            if Index (I).Attributes = Long_File_Name_Entry then
               Read_LFN :
               declare
                  Curr_File : Cxos.VFS.File_T
                    renames Directory_Files (File_Idx);

                  Name_Idx : Natural := 1;

                  LFN_Entry : constant Long_File_Name_Directory_Entry :=
                    Dir_Entry_To_LFN_Dir_Entry (Index (I));
               begin
                  Name_Idx := Natural (LFN_Entry.Sequence.Number) * 13;

                  Curr_File.File_Name (Name_Idx .. (Name_Idx + 4))
                    := LFN_Entry.Name_1 (1 .. 5);

                  Curr_File.File_Name ((Name_Idx + 5) .. (Name_Idx + 10))
                    := LFN_Entry.Name_2 (1 .. 6);

                  Curr_File.File_Name ((Name_Idx + 11) .. (Name_Idx + 12))
                    := LFN_Entry.Name_3 (1 .. 2);

               end Read_LFN;
            else
               Debug_Print ("SH: " & Index (I).File_Name);

               Cxos.Debug.
                 Put_String_Wide (Directory_Files (File_Idx).File_Name);
               Debug_Print ("" & Chars.LF);

               File_Idx := File_Idx + 1;
            end if;
         end loop;

      end Load_Index;

      Status := Success;
   exception
      when Constraint_Error =>
         Debug_Print ("Constraint error reading directory");
         null;
   end Parse_Directory;

end Cxos.Filesystems.FAT;
