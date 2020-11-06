with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1; use Ada.Characters.Wide_Latin_1;
with Cxos.Debug;
with Cxos.Error_Handling;
with Cxos.VFS;

package body Cxos.Filesystems.FAT is
   package Chars renames Ada.Characters.Latin_1;
   package Wide_Chars renames Ada.Characters.Wide_Latin_1;

   procedure Debug_Print_Wide (Data : Wide_String)
     renames Cxos.Debug.Put_String_Wide;
   --  Error logging function shorthand.
   procedure Log_Error (Message : String)
     renames Cxos.Error_Handling.Log_Kernel_Error;

   ----------------------------------------------------------------------------
   --  Get_DOS_Filename
   ----------------------------------------------------------------------------
   procedure Get_DOS_Filename (
     Dir_Entry :     Directory_Entry;
     Filename  : out Wide_String;
     Status    : out Program_Status
   ) is
      --  The length of the file's base name, without any extension.
      Name_Length : Natural := 0;
   begin
      Copy_Name :
      for Name_Idx in 1 .. 8 loop
         --  The DOS filename is padded by spaces.
         --  If we encounter a space we can assume we've reached the end
         --  of the file name.
         if Dir_Entry.File_Name (Name_Idx) = Chars.Space then
            exit Copy_Name;
         end if;

         Filename (Name_Idx) := Wide_Character'Val (Character'Pos (
           Dir_Entry.File_Name (Name_Idx)));

         --  Incrememnt the name length.
         Name_Length := Name_Length + 1;
      end loop Copy_Name;

      --  If this DOS file entry has an extension
      --  then it is appended after a trailing '.'
      --  character at the end of the file name.
      if Dir_Entry.File_Ext (1) /= Chars.Space then
         --  Place the '.' between the file name and
         --  the file extension.
         Place_Extension_Dot :
         begin
            Filename ((Name_Length + 1) .. (Name_Length + 1)) := ".";
         end Place_Extension_Dot;

         --  Copy the file extension into the filename.
         Copy_Extension :
         for Name_Idx in 1 .. 3 loop
            --  The DOS file extension is padded by spaces.
            --  If we encounter a space we can assume we've reached the end
            --  of the file name.
            if Dir_Entry.File_Ext (Name_Idx) = Chars.Space then
               exit Copy_Extension;
            end if;

            Filename (Name_Length + 1 + Name_Idx) :=
              Wide_Character'Val (Character'Pos (
              Dir_Entry.File_Ext (Name_Idx)));
         end loop Copy_Extension;
      end if;

      Status := Success;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint error reading LFN" & Chars.LF);
         Status := Unhandled_Exception;
   end Get_DOS_Filename;

   ----------------------------------------------------------------------------
   --  Get_Filesystem_Type
   ----------------------------------------------------------------------------
   procedure Get_Filesystem_Type (
     Boot_Sector :     Boot_Sector_T;
     FAT_Type    : out FAT_Type_T;
     Status      : out Program_Status
   ) is
      --  The extended BIOS parameter block, if the filesystem is FAT12/16.
      EBPB : Extended_BIOS_Parameter_Block
      with Import,
        Convention => Ada,
        Address    => Boot_Sector.BPB_Buffer'Address;

      --  The extended FAT32 BIOS parameter block,
      --  if the filesystem is FAT32.
      FAT32_EBPB : FAT32_Extended_BIOS_Parameter_Block
      with Import,
        Convention => Ada,
        Address    => Boot_Sector.BPB_Buffer'Address;

      --  The size of the FAT table.
      FAT_Size       : Unsigned_32 := 0;
      --  The total number of sectors.
      Total_Sectors  : Unsigned_32 := 0;
      --  The number of data sectors.
      Data_Sectors   : Unsigned_32 := 0;
      --  Total clusters.
      Total_Clusters : Unsigned_32 := 0;
   begin
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

      Status := Success;
   exception
      when Constraint_Error =>
         Status := Unhandled_Exception;
   end Get_Filesystem_Type;

   ----------------------------------------------------------------------------
   --  Get_Root_Directory_Sector
   ----------------------------------------------------------------------------
   procedure Get_Root_Directory_Sector (
     Boot_Sector           :     Boot_Sector_T;
     Root_Directory_Sector : out Natural;
     Status                : out Program_Status
   ) is
      --  The type of this FAT filesystem.
      Filesystem_Type : FAT_Type_T := FAT12;
   begin
      Get_Filesystem_Type (Boot_Sector, Filesystem_Type, Status);
      if Status /= Success then
         Root_Directory_Sector := 0;
         return;
      end if;

      if Filesystem_Type = FAT12 or Filesystem_Type = FAT16 then
         Get_Fat12_Root_Dir_Sector :
         declare
            --  The extended BIOS parameter block,
            --   if the filesystem is FAT12/16.
            EBPB : Extended_BIOS_Parameter_Block
            with Import,
              Convention => Ada,
              Address    => Boot_Sector.BPB_Buffer'Address;

            First_Data_Sector : Natural;
            Root_Dir_Sectors  : Natural;
         begin
            Root_Dir_Sectors := ((Natural (EBPB.BPB.Root_Entry_Count) * 32) +
              (Natural (EBPB.BPB.Bytes_Per_Sector)) - 1) /
              Natural (EBPB.BPB.Bytes_Per_Sector);

            First_Data_Sector := Natural (EBPB.BPB.Reserved_Sector_Count) +
              (Natural (EBPB.BPB.Table_Count) *
              Natural (EBPB.BPB.Table_Size)) +
              Root_Dir_Sectors;

            Root_Directory_Sector := First_Data_Sector - Root_Dir_Sectors;
         end Get_Fat12_Root_Dir_Sector;
      else
         Get_Fat32_Root_Dir_Sector :
         declare
            --  The extended FAT32 BIOS parameter block,
            --  if the filesystem is FAT32.
            FAT32_EBPB : FAT32_Extended_BIOS_Parameter_Block
            with Import,
              Convention => Ada,
              Address    => Boot_Sector.BPB_Buffer'Address;
         begin
            Root_Directory_Sector := Natural (FAT32_EBPB.Root_Cluster);
         end Get_Fat32_Root_Dir_Sector;
      end if;
   exception
      when Constraint_Error =>
         Status := Unhandled_Exception;
   end Get_Root_Directory_Sector;

   ----------------------------------------------------------------------------
   --  Is_Last_Directory_Entry
   ----------------------------------------------------------------------------
   function Is_Last_Directory_Entry (
     Dir_Entry : Directory_Entry
   ) return Boolean is
   begin
      return Dir_Entry.File_Name (1) = Chars.NUL;
   end Is_Last_Directory_Entry;

   ----------------------------------------------------------------------------
   --  Is_LFN_Entry
   ----------------------------------------------------------------------------
   function Is_LFN_Entry (
     Dir_Entry : Directory_Entry
   ) return Boolean is
   begin
      return Dir_Entry.Attributes.Read_Only and
        Dir_Entry.Attributes.Hidden and
        Dir_Entry.Attributes.System_Entry and
        Dir_Entry.Attributes.Volume_Label;
   exception
      when Constraint_Error =>
         return False;
   end Is_LFN_Entry;

   ----------------------------------------------------------------------------
   --  Is_Unused_Entry
   ----------------------------------------------------------------------------
   function Is_Unused_Entry (
     Dir_Entry : Directory_Entry
   ) return Boolean is
   begin
      return Character'Pos (Dir_Entry.File_Name (1)) = 16#E5#;
   end Is_Unused_Entry;

   ----------------------------------------------------------------------------
   --  Read_Directory
   ----------------------------------------------------------------------------
   procedure Read_Directory (
     Directory :     Directory_Index;
     Status    : out Program_Status
   ) is
      --  Whether a long file name entry is being read.
      Is_Scanning_LFN : Boolean := False;

      --  The index of the file being output.
      File_Idx          : Natural := 1;
      Directory_Entries : array (1 .. 128) of VFS.Directory_Entry_T;
   begin
      --  Since a sector is 512 bytes, the maximum number of entries in
      --  an individual directory sector is 16.
      Scan_Directory :
      for Dir_Idx in 1 .. 16 loop
         --  Exit loop if this is the last entry.
         if Is_Last_Directory_Entry (Directory (Dir_Idx)) then
            exit Scan_Directory;
         end if;

         if not Is_Unused_Entry (Directory (Dir_Idx)) then
            --  If the entry attributes indicate that this is a long
            --  file name entry, then parse it differently.
            if Is_LFN_Entry (Directory (Dir_Idx)) then
               Is_Scanning_LFN := True;

               --  Read this section of the name from the LFN entry.
               Read_LFN_Entry_Name (Directory (Dir_Idx),
                  Directory_Entries (File_Idx).Name, Status);
            else
               --  If we're not in the process of reading a long file name
               --  then read the DOS filename from the 8.3 entry.
               if Is_Scanning_LFN = False then
                  Get_DOS_Filename (Directory (Dir_Idx),
                     Directory_Entries (File_Idx).Name, Status);
               end if;

               --  Temporary logging of file name.
               Debug_Print_Wide ("* " & Directory_Entries (File_Idx).Name);
               File_Idx := File_Idx + 1;

               Is_Scanning_LFN := False;
            end if;
         end if;
      end loop Scan_Directory;

      Status := Success;
   exception
      when Constraint_Error =>
         Status := Failure;
   end Read_Directory;

   ----------------------------------------------------------------------------
   --  Read_LFN_Entry_Name
   ----------------------------------------------------------------------------
   procedure Read_LFN_Entry_Name (
     Dir_Entry :        Directory_Entry;
     Filename  : in out Wide_String;
     Status    :    out Program_Status
   ) is
      --  The long file name entry being parsed.
      LFN_Entry : Long_File_Name_Directory_Entry
      with Import,
        Convention => Ada,
        Address    => Dir_Entry'Address;

      --  The current offset into reading the name.
      Name_offset : Natural := 1;
   begin
      if not Is_LFN_Entry (Dir_Entry) then
         Status := Invalid_Argument;
         return;
      end if;

      --  The offset into the name is always the sequence number
      --  multiplied by the maximum string length that each
      --  entry holds, which is 13.
      Name_offset := Natural (LFN_Entry.Sequence.Number) * 13;

      Status := Success;

      --  Copy each of the string sections contained in this
      --  file name entry into the current directory entry.
      for I in 1 .. 5 loop
         --  If a NULL word is encountered, this signifies the end of the
         --  name. Stop the copying process and exit the function.
         if LFN_Entry.Name_1 (I) = Wide_Chars.NUL then
            return;
         end if;

         Filename (Name_offset + I) := LFN_Entry.Name_1 (I);
      end loop;

      for I in 1 .. 6 loop
         if LFN_Entry.Name_2 (I) = Wide_Chars.NUL then
            return;
         end if;

         Filename (Name_offset + 5 + I) := LFN_Entry.Name_2 (I);
      end loop;

      for I in 1 .. 2 loop
         if LFN_Entry.Name_3 (I) = Wide_Chars.NUL then
            return;
         end if;

         Filename (Name_offset + 11 + I) := LFN_Entry.Name_3 (I);
      end loop;
   exception
      when Constraint_Error =>
         Log_Error ("Constraint error reading LFN" & Chars.LF);
         Status := Failure;
   end Read_LFN_Entry_Name;

end Cxos.Filesystems.FAT;
