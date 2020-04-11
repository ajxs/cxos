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

with Cxos.Debug;

package body Cxos.ATA is
   use x86.ATA;

   ----------------------------------------------------------------------------
   --  Get_Device_Type
   ----------------------------------------------------------------------------
   function Get_Device_Type (
     Device_Type : out x86.ATA.ATA_Device_Type;
     Bus         :     x86.ATA.ATA_Bus;
     Position    :     x86.ATA.ATA_Device_Position
   ) return Process_Result is
      --  The result of internal processes.
      Result              : Process_Result;
      --  The cylinder low value.
      Drive_Cylinder_Low  : Unsigned_8;
      --  The cylinder high value.
      Drive_Cylinder_High : Unsigned_8;
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
      Drive_Cylinder_High := x86.ATA.
        Read_Byte_From_Register (Bus, Cylinder_High);
      Drive_Cylinder_Low  := x86.ATA.
        Read_Byte_From_Register (Bus, Cylinder_Low);

      if Drive_Cylinder_Low = 16#14# and Drive_Cylinder_High = 16#EB# then
         Device_Type := PATAPI;
      elsif Drive_Cylinder_Low = 16#69# and Drive_Cylinder_High = 16#96# then
         Device_Type := SATAPI;
      elsif Drive_Cylinder_Low = 16#3C# and Drive_Cylinder_High = 16#C3# then
         Device_Type := SATA;
      elsif Drive_Cylinder_Low = 0 and Drive_Cylinder_High = 0 then
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
      --  The raw status value read from the device.
      Status_Read_Value   : Unsigned_8;
      --  The device cylinder high value, used to find device type/status.
      Cylinder_High_Value : Unsigned_16;
      --  The device cylinder low value.
      Cylinder_Low_Value  : Unsigned_16;

      --  The result of internal processes.
      Result : Process_Result;
      --  Buffer to read the device identification info into.
      Identification_Buffer : Device_Identification_Buffer;
   begin
      Send_Identify_Command :
         begin
            --  Reset these to 0 as per the ATA spec.
            x86.ATA.Write_Word_To_Register (Bus, Sector_Count, 0);
            x86.ATA.Write_Word_To_Register (Bus, Sector_Number, 0);
            x86.ATA.Write_Word_To_Register (Bus, Cylinder_High, 0);
            x86.ATA.Write_Word_To_Register (Bus, Cylinder_Low, 0);

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

            --  Read the device status.
            Status_Read_Value := x86.ATA.
              Read_Byte_From_Register (Bus, Alt_Status);
            if Status_Read_Value = 0 then
               return Device_Not_Present;
            end if;

            Cylinder_High_Value :=  x86.ATA.
              Read_Word_From_Register (Bus, Cylinder_High);
            Cylinder_Low_Value  :=  x86.ATA.
              Read_Word_From_Register (Bus, Cylinder_Low);

            if (Cylinder_High_Value /= 0) or (Cylinder_Low_Value /= 0) then
               return Device_Non_ATA;
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
               Identification_Buffer (I) := x86.ATA.
                 Read_Word_From_Register (Bus, Data_Reg);
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

   procedure Initialise is
      Device_Type : x86.ATA.ATA_Device_Type;
      Result      : Process_Result;
      Rec : Device_Identification_Record;
   begin
      Result := Reset_Bus (Primary);
      if Result /= Success then
         Cxos.Debug.Put_String ("Error resetting device" & ASCII.LF);
      end if;

      Result := Get_Device_Type (Device_Type, Primary, Master);
      if Result /= Success then
         Cxos.Debug.Put_String ("Error reading device type" & ASCII.LF);
      end if;

      case Device_Type is
         when PATAPI =>
            Cxos.Debug.Put_String ("PATAPI" & ASCII.LF);
         when SATAPI =>
            Cxos.Debug.Put_String ("PATAPI" & ASCII.LF);
         when PATA   =>
            Cxos.Debug.Put_String ("PATA" & ASCII.LF);
         when SATA   =>
            Cxos.Debug.Put_String ("SATA" & ASCII.LF);
         when Unknown_ATA_Device =>
            Cxos.Debug.Put_String ("Unknown" & ASCII.LF);
      end case;

      Result := Identify (Rec, Primary, Master);
      if Result /= Success then
         Cxos.Debug.Put_String ("Error identifying device" & ASCII.LF);
      end if;

      if Rec.Device_Config.Removable_Media = True then
         Cxos.Debug.Put_String ("Removable Media" & ASCII.LF);
      end if;

      Result := Get_Device_Type (Device_Type, Primary, Slave);
      if Result /= Success then
         Cxos.Debug.Put_String ("Error reading device type" & ASCII.LF);
      end if;

      case Device_Type is
         when PATAPI =>
            Cxos.Debug.Put_String ("PATAPI" & ASCII.LF);
         when SATAPI =>
            Cxos.Debug.Put_String ("PATAPI" & ASCII.LF);
         when PATA   =>
            Cxos.Debug.Put_String ("PATA" & ASCII.LF);
         when SATA   =>
            Cxos.Debug.Put_String ("SATA" & ASCII.LF);
         when Unknown_ATA_Device =>
            Cxos.Debug.Put_String ("Unknown" & ASCII.LF);
      end case;
   exception
      when Constraint_Error =>
         return;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Read_Word
   ----------------------------------------------------------------------------
   function Read_Word (
     Data : out Unsigned_16;
     Bus  :     x86.ATA.ATA_Bus
   ) return Process_Result is
      Result : Process_Result;
   begin
      Result := Send_Command (Bus, Read_Long_Retry);
      if Result /= Success then
         return Result;
      end if;

      Data := 1;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Read_Word;

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
      use Cxos.Time_Keeping;

      --  The system time at the start of the timeout.
      Start_Time : Cxos.Time_Keeping.Time;
   begin
      case Position is
         when Master =>
            x86.ATA.Write_Byte_To_Register (Bus, Drive_Head, 16#A0#);
         when Slave  =>
            x86.ATA.Write_Byte_To_Register (Bus, Drive_Head, 16#B0#);
      end case;

      --  Delay 400ns post drive selection, as per spec.
      Drive_Select_Delay :
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
               when Identify_Device =>
                  Command_Byte := 16#EC#;
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
      --  The current system time.
      Current_Time    : Cxos.Time_Keeping.Time;
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
            Check_Timeout :
               begin
                  Current_Time := Cxos.Time_Keeping.Clock;
                  if (Current_Time - Start_Time) > Timeout then
                     exit Wait_While_Busy;
                  end if;
               end Check_Timeout;
         end loop Wait_While_Busy;

      --  If no value has been returned within the attempt threshold,
      --  return this status.
      return Device_Busy;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Wait_For_Device_Ready;
end Cxos.ATA;
