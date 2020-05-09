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
with Cxos.Debug;

package body Cxos.Devices.ATA is
   package Chars renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------------
   --  Find_ATA_Devices
   ----------------------------------------------------------------------------
   procedure Find_ATA_Devices is
      Result      : Process_Result;
      Device_Idx  : Natural := 0;
   begin
      Initialise_Device_Array :
         for I in ATA_Devices'Range loop
            ATA_Devices (I).Present := False;
         end loop Initialise_Device_Array;

      Bus_Loop :
         for Bus in ATA_Bus'Range loop
            Result := Reset_Bus (Bus);
            if Result /= Success then
               Cxos.Debug.Put_String ("Error resetting ATA bus: ");
               Print_Process_Result (Result);
               Cxos.Debug.Put_String ("" & Chars.LF);

               return;
            end if;

            --  Iterate over primary and secondary buses.
            Position_Loop :
               for Position in ATA_Device_Position'Range loop
                  --  Read the invidual ATA device at this position.
                  Result := Read_ATA_Device_Info (ATA_Devices (Device_Idx),
                    Bus, Position);
                  if Result /= Success then
                     Cxos.Debug.Put_String ("Error reading ATA device: ");
                     Print_Process_Result (Result);
                     Cxos.Debug.Put_String ("" & Chars.LF);
                  else
                     Device_Idx := Device_Idx + 1;
                  end if;

               end loop Position_Loop;
         end loop Bus_Loop;

      --  Print information on found devices.
      Print_Device_Info :
         for I in Natural range 0 .. 7 loop
            if ATA_Devices (I).Present then
               Print_ATA_Device (ATA_Devices (I));
            end if;
         end loop Print_Device_Info;
   exception
      when Constraint_Error =>
         return;
   end Find_ATA_Devices;

   ----------------------------------------------------------------------------
   --  Flush_Write_Cache
   ----------------------------------------------------------------------------
   function Flush_Bus_Write_Cache (
     Bus : x86.ATA.ATA_Bus
   ) return Process_Result is
      --  The result of internal processes.
      Result : Process_Result;
   begin
      Result := Send_Command (Bus, Flush_Write_Cache);
      if Result /= Success then
         return Result;
      end if;

      --  Wait until the device is ready to receive commands.
      Result := Wait_For_Device_Ready (Bus, 10000);
      if Result /= Success then
         return Result;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Flush_Bus_Write_Cache;

   ----------------------------------------------------------------------------
   --  Get_Device_Type
   ----------------------------------------------------------------------------
   function Get_Device_Type (
     Device_Type : out x86.ATA.ATA_Device_Type;
     Bus         :     x86.ATA.ATA_Bus;
     Position    :     x86.ATA.ATA_Device_Position
   ) return Process_Result is
      --  The result of internal processes.
      Result            : Process_Result;
      --  The cylinder high value.
      Cylinder_High_Val : Unsigned_8;
      --  The cylinder low value.
      Cylinder_Low_Val  : Unsigned_8;
   begin
      --  Select the master/slave device.
      Result := Select_Device_Position (Bus, Position);
      if Result /= Success then
         return Result;
      end if;

      --  Wait until the device is ready to receive commands.
      Result := Wait_For_Device_Ready (Bus, 10000);
      if Result /= Success then
         return Result;
      end if;

      --  Read device identification info.
      Cylinder_High_Val := Read_Byte_From_Register (Bus, Cylinder_High);
      Cylinder_Low_Val  := Read_Byte_From_Register (Bus, Cylinder_Low);

      if Cylinder_Low_Val = 16#14# and Cylinder_High_Val = 16#EB# then
         Device_Type := PATAPI;
      elsif Cylinder_Low_Val = 16#69# and Cylinder_High_Val = 16#96# then
         Device_Type := SATAPI;
      elsif Cylinder_Low_Val = 16#3C# and Cylinder_High_Val = 16#C3# then
         Device_Type := SATA;
      elsif Cylinder_Low_Val = 0 and Cylinder_High_Val = 0 then
         Device_Type := PATA;
      else
         Device_Type := Unknown_ATA_Device;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Get_Device_Type;

   ----------------------------------------------------------------------------
   --  Identify
   ----------------------------------------------------------------------------
   function Identify (
     Id_Record : out x86.ATA.Device_Identification_Record;
     Bus       :     x86.ATA.ATA_Bus;
     Position  :     x86.ATA.ATA_Device_Position
   ) return Process_Result is
      --  The result of internal processes.
      Result : Process_Result;
      --  Buffer to read the device identification info into.
      Identification_Buffer : Device_Identification_Buffer;
   begin
      Send_Identify_Command :
         declare
            --  The device cylinder high value, used to find
            --  device type/status.
            Cylinder_High_Val : Unsigned_8;
            --  The device cylinder low value.
            Cylinder_Low_Val  : Unsigned_8;
         begin
            --  Reset these to 0 as per the ATA spec.
            Write_Word_To_Register (Bus, Sector_Count_Reg, 0);
            Write_Word_To_Register (Bus, Sector_Number_Reg, 0);
            Write_Word_To_Register (Bus, Cylinder_High, 0);
            Write_Word_To_Register (Bus, Cylinder_Low, 0);

            --  Select the master/slave device.
            Result := Select_Device_Position (Bus, Position);
            if Result /= Success then
               return Result;
            end if;

            --  Send the identify command.
            Result := Send_Command (Bus, Identify_Device);
            if Result /= Success then
               return Result;
            end if;

            --  Read the device status to check if it is present.
            Check_Device_Status :
               declare
                  --  The raw status value read from the device.
                  Status_Value : Unsigned_8;
               begin
                  Status_Value := Read_Byte_From_Register (Bus, Alt_Status);
                  if Status_Value = 0 then
                     return Device_Not_Present;
                  end if;
               end Check_Device_Status;

            --  Read the device status until the device is ready.
            Result := Wait_For_Device_Ready (Bus);
            if Result /= Success then
               return Result;
            end if;

            Cylinder_High_Val := Read_Byte_From_Register (Bus, Cylinder_High);
            Cylinder_Low_Val  := Read_Byte_From_Register (Bus, Cylinder_Low);

            if (Cylinder_High_Val /= 0) or (Cylinder_Low_Val /= 0) then
               return Device_Non_ATA;
            end if;

         end Send_Identify_Command;

      --  Read in the identification buffer.
      Read_Identification :
         begin
            for I in Integer range 0 .. 255 loop
               Identification_Buffer (I) := Read_Word_From_Register (Bus,
                 Data_Reg);
            end loop;

            --  Convert the raw buffer to the identification record.
            Id_Record := Device_Identification_Buffer_To_Record (
              Identification_Buffer);
         end Read_Identification;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Identify;

   ----------------------------------------------------------------------------
   --  Identify_Packet_Device
   ----------------------------------------------------------------------------
   function Identify_Packet_Device (
     Id_Record : out x86.ATA.Device_Identification_Record;
     Bus       :     x86.ATA.ATA_Bus;
     Position  :     x86.ATA.ATA_Device_Position
   ) return Process_Result is
      --  The result of internal processes.
      Result : Process_Result;
      --  Buffer to read the device identification info into.
      Identification_Buffer : Device_Identification_Buffer;
   begin
      Send_Identify_Command :
         declare
            --  The device cylinder high value, used to find
            --  device type/status.
            Cylinder_High_Val : Unsigned_8;
            --  The device cylinder low value.
            Cylinder_Low_Val  : Unsigned_8;
         begin
            --  Reset these to 0 as per the ATA spec.
            Write_Byte_To_Register (Bus, Sector_Count_Reg, 0);
            Write_Byte_To_Register (Bus, Sector_Number_Reg, 0);
            Write_Byte_To_Register (Bus, Cylinder_High, 0);
            Write_Byte_To_Register (Bus, Cylinder_Low, 0);

            --  Select the master/slave device.
            Result := Select_Device_Position (Bus, Position);
            if Result /= Success then
               return Result;
            end if;

            --  Send the identify command.
            Result := Send_Command (Bus, Identify_Device);
            if Result /= Success then
               return Result;
            end if;

            --  Read the device status to check if it is present.
            Check_Device_Status :
               declare
                  --  The raw status value read from the device.
                  Status_Value : Unsigned_8;
               begin
                  Status_Value := Read_Byte_From_Register (Bus, Alt_Status);
                  if Status_Value = 0 then
                     return Device_Not_Present;
                  end if;
               end Check_Device_Status;

            Cylinder_High_Val := Read_Byte_From_Register (Bus, Cylinder_High);
            Cylinder_Low_Val  := Read_Byte_From_Register (Bus, Cylinder_Low);

            if (Cylinder_High_Val /= 16#EB#) and (Cylinder_Low_Val /= 16#14#)
            then
               return Packet_Interface_Not_Supported;
            end if;

            --  Read the device status until the device is ready.
            Result := Wait_For_Device_Ready (Bus);
            if Result /= Success then
               return Result;
            end if;
         end Send_Identify_Command;

      --  Read in the identification buffer.
      Read_Identification :
         begin
            for I in Integer range 0 .. 255 loop
               Identification_Buffer (I) := Read_Word_From_Register (Bus,
                 Data_Reg);
            end loop;

            --  Convert the raw buffer to the identification record.
            Id_Record := Device_Identification_Buffer_To_Record (
              Identification_Buffer);
         end Read_Identification;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Identify_Packet_Device;

   ----------------------------------------------------------------------------
   --  Print_ATA_Device
   ----------------------------------------------------------------------------
   procedure Print_ATA_Device (
     Device : ATA_Device
   ) is
   begin
      Cxos.Debug.Put_String ("------------------------" & Chars.LF);
      Cxos.Debug.Put_String ("IDE Device: " & Chars.LF);

      Print_Bus_Info :
         begin
            Cxos.Debug.Put_String ("  Bus:               ");
            case Device.Bus is
               when Primary =>
                  Cxos.Debug.Put_String ("Primary" & Chars.LF);
               when Secondary =>
                  Cxos.Debug.Put_String ("Secondary" & Chars.LF);
            end case;

            Cxos.Debug.Put_String ("  Position:          ");
            case Device.Position is
               when Master =>
                  Cxos.Debug.Put_String ("Master" & Chars.LF);
               when Slave =>
                  Cxos.Debug.Put_String ("Slave" & Chars.LF);
            end case;

         end Print_Bus_Info;

      Print_Device_Type :
         begin
            Cxos.Debug.Put_String ("  Type:              ");
            case Device.Device_Type is
               when PATAPI =>
                  Cxos.Debug.Put_String ("PATAPI" & Chars.LF);
               when SATAPI =>
                  Cxos.Debug.Put_String ("PATAPI" & Chars.LF);
               when PATA   =>
                  Cxos.Debug.Put_String ("PATA" & Chars.LF);
               when SATA   =>
                  Cxos.Debug.Put_String ("SATA" & Chars.LF);
               when Unknown_ATA_Device =>
                  Cxos.Debug.Put_String ("Unknown" & Chars.LF);
            end case;
         end Print_Device_Type;

      Print_Device_Info :
         begin
            if Device.Identification.Device_Config.Removable_Media = True then
               Cxos.Debug.Put_String ("  Device is removable Media"
                 & Chars.LF);
            end if;
         end Print_Device_Info;

      Print_Firmware_Revision :
         begin
            Cxos.Debug.Put_String ("  Firmware revision: '");

            --  ATA Strings are in partially reverse order. Each pair of
            --  characters has its order reversed.
            for C in Natural range 0 .. 3 loop
               Cxos.Debug.Put_String ("" &
                 Device.Identification.Firmware_Revision ((C * 2) + 1) &
                 Device.Identification.Firmware_Revision (C * 2));
            end loop;
            Cxos.Debug.Put_String ("'" & Chars.LF);
         end Print_Firmware_Revision;

      Print_Model_Number :
         begin
            Cxos.Debug.Put_String ("  Model number:      '");
            for C in Natural range 0 .. 19 loop
               Cxos.Debug.Put_String ("" &
                 Device.Identification.Model_Number ((C * 2) + 1) &
                 Device.Identification.Model_Number (C * 2));
            end loop;
            Cxos.Debug.Put_String ("'" & Chars.LF);
         end Print_Model_Number;

      Print_Serial_Number :
         begin
            Cxos.Debug.Put_String ("  Serial number:     '");
            for C in Natural range 0 .. 9 loop
               Cxos.Debug.Put_String ("" &
                 Device.Identification.Serial_Number ((C * 2) + 1) &
                 Device.Identification.Serial_Number (C * 2));
            end loop;
            Cxos.Debug.Put_String ("'" & Chars.LF);
         end Print_Serial_Number;

      Print_Command_Set :
         begin
            Cxos.Debug.Put_String ("  Command set:" & Chars.LF);
            Cxos.Debug.Put_String ("    LBA48:           ");
            if Device.Identification.Command_Set_Support.LBA48 then
               Cxos.Debug.Put_String ("Yes" & Chars.LF);
            else
               Cxos.Debug.Put_String ("No" & Chars.LF);
            end if;
         end Print_Command_Set;
   exception
      when Constraint_Error =>
         return;
   end Print_ATA_Device;

   ----------------------------------------------------------------------------
   --  Print_Process_Result
   ----------------------------------------------------------------------------
   procedure Print_Process_Result (
     Result : Process_Result
   ) is
   begin
      case Result is
         when Command_Aborted =>
            Cxos.Debug.Put_String ("Command aborted");
         when Device_Busy =>
            Cxos.Debug.Put_String ("Device Busy");
         when Device_Error_State =>
            Cxos.Debug.Put_String ("Device is in error state");
         when Device_Non_ATA =>
            Cxos.Debug.Put_String ("Device is non-ATA");
         when Device_Not_Present =>
            Cxos.Debug.Put_String ("Device not present");
         when Invalid_Command =>
            Cxos.Debug.Put_String ("Invalid command");
         when Packet_Interface_Not_Supported =>
            Cxos.Debug.Put_String ("Packet interface not supported");
         when Success =>
            Cxos.Debug.Put_String ("Success");
         when Unhandled_Exception =>
            Cxos.Debug.Put_String ("Unhandled Exception");
         when others =>
            Cxos.Debug.Put_String ("Other");
      end case;
   exception
      when Constraint_Error =>
         null;
   end Print_Process_Result;

   ----------------------------------------------------------------------------
   --  Read_ATA_Device
   ----------------------------------------------------------------------------
   function Read_ATA_Device (
     Bus        :     x86.ATA.ATA_Bus;
     Position   :     x86.ATA.ATA_Device_Position;
     Sector_Cnt :     x86.ATA.ATA_Sector_Count;
     LBA        :     x86.ATA.ATA_LBA;
     Buffer     : out ATA_Buffer
   ) return Process_Result is
      Drive_Register_Val : Unsigned_8;
      Result : Process_Result;
   begin
      case Position is
         when Master =>
            Drive_Register_Val := 16#A0#;
         when Slave  =>
            Drive_Register_Val := 16#B0#;
      end case;

      Drive_Register_Val := Drive_Register_Val or 16#40#;
      x86.ATA.Write_Byte_To_Register (Bus, Drive_Head, Drive_Register_Val);

      --  Delay 400ns post drive selection, as per spec.
      Drive_Select_Delay :
         declare
            use Cxos.Time_Keeping;

            --  The system time at the start of the timeout.
            Start_Time : Cxos.Time_Keeping.Time;
         begin
            Start_Time := Cxos.Time_Keeping.Clock;
            while (Cxos.Time_Keeping.Clock - Start_Time) < 1 loop
               null;
            end loop;
         end Drive_Select_Delay;

      --  Send the sector count.
      Send_Sector_Count :
         declare
            Sector_Count_Byte : Unsigned_8;
         begin
            Sector_Count_Byte := Unsigned_8 (Sector_Cnt);
            Write_Byte_To_Register (Bus, Sector_Count_Reg, Sector_Count_Byte);
         end Send_Sector_Count;

      Set_LBA :
         declare
            LBA_U : Unsigned_32;
            LBA_Byte : Unsigned_8;
         begin
            LBA_U := Unsigned_32 (LBA and 16#FFFFFFFF#);

            LBA_Byte := Unsigned_8 (LBA_U and 16#FF#);
            Write_Byte_To_Register (Bus, Sector_Number_Reg, LBA_Byte);

            LBA_Byte := Unsigned_8 (Shift_Right (LBA_U, 8) and 16#FF#);
            Write_Byte_To_Register (Bus, Sector_Count_Reg, LBA_Byte);

            LBA_Byte := Unsigned_8 (Shift_Right (LBA_U, 16) and 16#FF#);
            Write_Byte_To_Register (Bus, Sector_Count_Reg, LBA_Byte);
         end Set_LBA;

      Result := Send_Command (Bus, Read_Sectors_Retry);
      if Result /= Success then
         Cxos.Debug.Put_String ("Error sending read command: ");
         Print_Process_Result (Result);
         Cxos.Debug.Put_String ("" & Chars.LF);

         return Result;
      end if;

      Check_Device :
         declare
            Status_Byte : Unsigned_8;
         begin
            Result := Wait_For_Device_Ready (Bus);
            if Result /= Success then
               Cxos.Debug.Put_String ("Error waiting for device ready: ");
               Print_Process_Result (Result);
               Cxos.Debug.Put_String ("" & Chars.LF);

               return Result;
            end if;

            Status_Byte := Read_Byte_From_Register (Bus, Alt_Status);
            Cxos.Debug.Put_String ("Status: " & Status_Byte'Image & Chars.LF);
         end Check_Device;

      Read_Buffer :
         declare
            Read_Count : Natural;
         begin
            if Sector_Cnt = 0 then
               Read_Count := 256;
            else
               Read_Count := Natural (Sector_Cnt);
            end if;

            for I in Natural range 0 .. (Read_Count - 1) loop
               Buffer (I) := Read_Word_From_Register (Bus, Data_Reg);
            end loop;
         end Read_Buffer;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Read_ATA_Device;

   ----------------------------------------------------------------------------
   --  Read_ATA_Device_Info
   ----------------------------------------------------------------------------
   function Read_ATA_Device_Info (
     Device   : out ATA_Device;
     Bus      :     ATA_Bus;
     Position :   ATA_Device_Position
   ) return Process_Result is
      Device_Type : x86.ATA.ATA_Device_Type;
      Result      : Process_Result;
      Id_Record   : Device_Identification_Record;
   begin
      Result := Identify (Id_Record, Bus, Position);
      case Result is
         when Success =>
            Device := (
              Present        => True,
              Bus            => Bus,
              Position       => Position,
              Device_Type    => PATA,
              Identification => Id_Record
            );

            return Success;
         when Device_Not_Present =>
            null;
         when Device_Non_ATA =>
            Device_Type := Unknown_ATA_Device;

            --  If the drive is non-ATA.
            Result := Get_Device_Type (Device_Type, Bus, Position);
            if Result /= Success then
               Cxos.Debug.Put_String ("Error reading device type: ");
               Print_Process_Result (Result);
               Cxos.Debug.Put_String ("" & Chars.LF);

               return Result;
            end if;

            --  If this is a packet device, identify.
            if Device_Type = PATAPI or Device_Type = SATAPI then
               Result := Identify_Packet_Device (Id_Record, Bus, Position);
               if Result /= Success then
                  Cxos.Debug.Put_String ("Error identifying device: ");
                  Print_Process_Result (Result);
                  Cxos.Debug.Put_String ("" & Chars.LF);

                  return Result;
               end if;

               Device := (
                 Present        => True,
                 Bus            => Bus,
                 Position       => Position,
                 Device_Type    => Device_Type,
                 Identification => Id_Record
               );
            else
               Cxos.Debug.Put_String ("Other non ATA?" & Chars.LF);
            end if;
         when others =>
            Cxos.Debug.Put_String ("Error identifying device: ");
            Print_Process_Result (Result);
            Cxos.Debug.Put_String ("" & Chars.LF);

            return Result;
      end case;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Read_ATA_Device_Info;

   ----------------------------------------------------------------------------
   --  Reset_Bus
   ----------------------------------------------------------------------------
   function Reset_Bus (
     Bus : x86.ATA.ATA_Bus
   ) return Process_Result is
   begin
      x86.ATA.Write_Byte_To_Register (Bus, Device_Control, 4);
      x86.ATA.Write_Byte_To_Register (Bus, Device_Control, 0);

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Reset_Bus;

   ----------------------------------------------------------------------------
   --  Select_Device_Position
   --
   --  Implementation Notes:
   --    - Introduces an articial 1ms delay after selecting the device to
   --      ensure that the correct device is selected.
   ----------------------------------------------------------------------------
   function Select_Device_Position (
     Bus      : x86.ATA.ATA_Bus;
     Position : x86.ATA.ATA_Device_Position
   ) return Process_Result is
   begin
      case Position is
         when Master =>
            x86.ATA.Write_Byte_To_Register (Bus, Drive_Head, 16#A0#);
         when Slave  =>
            x86.ATA.Write_Byte_To_Register (Bus, Drive_Head, 16#B0#);
      end case;

      --  Delay 400ns post drive selection, as per spec.
      Drive_Select_Delay :
         declare
            use Cxos.Time_Keeping;

            --  The system time at the start of the timeout.
            Start_Time : Cxos.Time_Keeping.Time;
         begin
            Start_Time := Cxos.Time_Keeping.Clock;
            while (Cxos.Time_Keeping.Clock - Start_Time) < 1 loop
               null;
            end loop;
         end Drive_Select_Delay;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Select_Device_Position;

   ----------------------------------------------------------------------------
   --  Send_Command
   ----------------------------------------------------------------------------
   function Send_Command (
     Bus          : x86.ATA.ATA_Bus;
     Command_Type : x86.ATA.ATA_Command
   ) return Process_Result is
      --  The byte value to send.
      Command_Byte     : Unsigned_8;
   begin
      --  Set the command byte to send.
      Set_Command_Byte :
         begin
            case Command_Type is
               when Nop =>
                  Command_Byte := 0;
               when Device_Reset =>
                  Command_Byte := 16#08#;
               when Flush_Write_Cache =>
                  Command_Byte := 16#E7#;
               when Identify_Device =>
                  Command_Byte := 16#EC#;
               when Identify_Packet_Device =>
                  Command_Byte := 16#A1#;
               when Recalibrate =>
                  Command_Byte := 16#10#;
               when Read_Sectors_Retry =>
                  Command_Byte := 16#20#;
               when Read_Sectors_No_Retry =>
                  Command_Byte := 16#21#;
               when Read_Long_Retry =>
                  Command_Byte := 16#22#;
               when Read_Long_No_Retry =>
                  Command_Byte := 16#23#;
               when Read_Sectors_Ext =>
                  Command_Byte := 16#24#;
               when Read_DMA_Ext =>
                  Command_Byte := 16#25#;
               when others =>
                  return Invalid_Command;
            end case;
         end Set_Command_Byte;

      --  Send Command.
      x86.ATA.Write_Byte_To_Register (Bus, Command_Reg, Command_Byte);

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Send_Command;

   ----------------------------------------------------------------------------
   --  Wait_For_Device_Ready
   ----------------------------------------------------------------------------
   function Wait_For_Device_Ready (
     Bus     : x86.ATA.ATA_Bus;
     Timeout : Cxos.Time_Keeping.Time := 2000
   ) return Process_Result is
      use Cxos.Time_Keeping;

      --  The status value read from the device.
      Drive_Status    : x86.ATA.Device_Status_Record;
      --  The system time at the start of the function.
      Start_Time      : Cxos.Time_Keeping.Time;
   begin
      --  Get the start time.
      Start_Time := Cxos.Time_Keeping.Clock;

      --  Read the device status register in a loop until either the
      --  timeout is exceeded and the function exits, or a non-busy
      --  status is read.
      Wait_While_Busy :
         loop
            --  Read device status.
            Drive_Status := x86.ATA.Unsigned_8_To_Device_Status_Record (
              x86.ATA.Read_Byte_From_Register (Bus, Alt_Status));
            if Drive_Status.BSY = False then
               return Success;
            end if;

            --  If an error state is reported, exit here.
            if Drive_Status.ERR = True then
               return Device_Error_State;
            end if;

            --  Check to see whether we have exceeded the timeout threshold.
            --  If so, exit the loop.
            exit Wait_While_Busy
              when (Cxos.Time_Keeping.Clock - Start_Time) > Timeout;
         end loop Wait_While_Busy;

      --  If no value has been returned within the attempt threshold,
      --  return this status.
      return Device_Busy;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Wait_For_Device_Ready;

end Cxos.Devices.ATA;
