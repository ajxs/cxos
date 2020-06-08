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

with Ada.Characters.Latin_1;
with Interfaces;
with Cxos.Debug;
with Cxos.Devices.ATA;
with Cxos.Devices.PCI;
with Cxos.Filesystems.FAT;
with x86.ATA;

package body Cxos.Devices is
   package Chars renames Ada.Characters.Latin_1;
   procedure Debug_Print (Data : String) renames Cxos.Debug.Put_String;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
      use x86.ATA;

      --  The result of internal functionality.
      Result : Process_Result;

      Read_Buf    : Cxos.Devices.ATA.ATA_Read_Buffer (0 .. 255);
   begin
      Result := Cxos.Devices.PCI.Find_PCI_Devices;
      if Result /= Success then
         return;
      end if;

      Cxos.Devices.ATA.Find_ATA_Devices;

      Result := Cxos.Devices.ATA.Read_ATA_Device (Primary, Slave,
        1, 0, Read_Buf);
      if Result /= Success then
         Debug_Print ("Read Error." & Ada.Characters.Latin_1.LF);
         return;
      end if;

      Read_FS :
      declare
         use Cxos.Filesystems.FAT;
         use Interfaces;

         FAT_Type : FAT_Type_T := FAT12;

         Status   : Cxos.Filesystems.FAT.Program_Status;

         B_Sec : Cxos.Filesystems.FAT.Boot_Sector
         with Import,
           Convention => Ada,
           Address    => Read_Buf'Address;

         --  The extended BIOS parameter block, if the filesystem is FAT12/16.
         EBPB : Extended_BIOS_Parameter_Block
         with Import,
           Convention => Ada,
           Address    => B_Sec.BPB_Buffer'Address;
      begin
         Debug_Print ("" & Chars.LF);
         Cxos.Filesystems.FAT.Print_Filesystem_Info (B_Sec);

         Get_Filesystem_Type (B_Sec, FAT_Type, Status);
         if Status /= Success then
            Debug_Print ("Error getting filesystem type" & Chars.LF);
         end if;

         Read_Directory :
         declare
            Directory_LBA : ATA_LBA := 0;
            FAT_Buf    : Cxos.Devices.ATA.ATA_Read_Buffer (0 .. 1023);
         begin
            Directory_LBA := ATA_LBA ((EBPB.BPB.Table_Size *
              Unsigned_16 (EBPB.BPB.Table_Count)) +
              EBPB.BPB.Reserved_Sector_Count);

            Result := Cxos.Devices.ATA.Read_ATA_Device (Primary,
              Slave, 4, Directory_LBA, FAT_Buf);
            if Result /= Success then
               Debug_Print ("Read Error." & Chars.LF);
               return;
            end if;

            Parse_Directory (FAT_Buf'Address, 4, Status);
            if Status /= Success then
               return;
            end if;

         end Read_Directory;
      end Read_FS;

   exception
      when Constraint_Error =>
         Debug_Print ("Constraint Error" & Chars.LF);
         return;
   end Initialise;

end Cxos.Devices;
