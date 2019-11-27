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

with Cxos.Serial;

package body Cxos.PCI is
   use x86.PCI;

   ----------------------------------------------------------------------------
   --  Find_Pci_Devices
   ----------------------------------------------------------------------------
   function Find_Pci_Devices return Kernel_Process_Result is
      --  Variable to store the last read device.
      Device_Info : Pci_Device;
      --  Variable for testing whether a particular bus address contains
      --  a device.
      Test_Result : Boolean;
      --  The result of internal processes.
      Result      : Process_Result;
      --  Whether debug device info should be printed to serial out.
      PRINT_INFO  : constant Boolean := True;
   begin
      Cxos.Serial.Put_String ("Testing PCI Bus" & ASCII.LF);

      for Bus in Unsigned_8 range 0 .. 255 loop
         for Device in x86.PCI.Pci_Device_Number range 0 .. 31 loop
            Result := Test_Pci_Device (Test_Result, Bus, Device, 0);
            if Result /= Success then
               Cxos.Serial.Put_String ("Error testing PCI device" & ASCII.LF);
               return Failure;
            end if;

            if Test_Result then
               Result := Read_Pci_Device (Device_Info, Bus, Device, 0);
               if Result /= Success then
                  Cxos.Serial.Put_String ("Error reading PCI device" &
                    ASCII.LF);
                  return Failure;
               end if;

               if PRINT_INFO then
                  Print_Pci_Device (Device_Info);
               end if;

               --  If this is a multi-function device read any child devices
               --  on the function bus.
               if (Device_Info.Header_Type and 16#80#) /= 0 then
                  for Func in Pci_Function_Number range 1 .. 7 loop
                     Result := Test_Pci_Device (Test_Result, Bus,
                       Device, Func);
                     if Result /= Success then
                        Cxos.Serial.Put_String ("Error testing PCI device" &
                          ASCII.LF);
                        return Failure;
                     end if;

                     if Test_Result then
                        Result := Read_Pci_Device (Device_Info, Bus,
                          Device, Func);
                        if Result /= Success then
                           Cxos.Serial.Put_String ("Error reading PCI device" &
                             ASCII.LF);
                           return Failure;
                        end if;

                        if PRINT_INFO then
                           Print_Pci_Device (Device_Info);
                        end if;
                     end if;
                  end loop;
               end if;
            end if;
         end loop;
      end loop;

      return Success;
   exception
      when Constraint_Error =>
         return Failure;
   end Find_Pci_Devices;

   ----------------------------------------------------------------------------
   --  Print_Pci_Device
   ----------------------------------------------------------------------------
   procedure Print_Pci_Device (
      Device : Pci_Device
   ) is
   begin
      Cxos.Serial.Put_String ("------------------------" & ASCII.LF);
      Cxos.Serial.Put_String ("Device:" & ASCII.LF);
      Cxos.Serial.Put_String ("  Bus:       "
        & Device.Bus_Number'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  Device:    "
        & Device.Device_Number'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  Function:  "
        & Device.Function_Number'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  Vendor ID: "
        & Device.Vendor_Id'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  Device ID: "
        & Device.Device_Id'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  Class:      ");
      case Device.Device_Class is
         when 1 =>
            Cxos.Serial.Put_String ("Mass Storage Controller" & ASCII.LF);
            Cxos.Serial.Put_String ("  Subclass:   ");
            case Device.Subclass is
               when 1 =>
                  Cxos.Serial.Put_String ("IDE Controller" & ASCII.LF);
               when 2 =>
                  Cxos.Serial.Put_String ("Floppy Disk Controller" & ASCII.LF);
               when 3 =>
                  Cxos.Serial.Put_String ("IPI Bus Controller" & ASCII.LF);
               when 4 =>
                  Cxos.Serial.Put_String ("RAID Controller" & ASCII.LF);
               when 5 =>
                  Cxos.Serial.Put_String ("ATA Controller" & ASCII.LF);
               when 6 =>
                  Cxos.Serial.Put_String ("Serial ATA" & ASCII.LF);
               when 7 =>
                  Cxos.Serial.Put_String ("Serial Attached SCSI" & ASCII.LF);
               when 8 =>
                  Cxos.Serial.Put_String ("Non-Volatile Memory Controller"
                    & ASCII.LF);
               when 16#80# =>
                  Cxos.Serial.Put_String ("Other" & ASCII.LF);
               when others =>
                  Cxos.Serial.Put_String ("Unknown: "
                    & Device.Device_Class'Image & ASCII.LF);
            end case;
         when others =>
            Cxos.Serial.Put_String (Device.Device_Class'Image & ASCII.LF);
            Cxos.Serial.Put_String ("  Subclass:  "
              & Device.Subclass'Image & ASCII.LF);
      end case;
      Cxos.Serial.Put_String ("  Header:    "
        & Device.Header_Type'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  BAR0:      "
        & Device.BAR0'Image & ASCII.LF);

   end Print_Pci_Device;

   ----------------------------------------------------------------------------
   --  Read_Pci_Device
   ----------------------------------------------------------------------------
   function Read_Pci_Device (
     Device          : out Pci_Device;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     Pci_Device_Number;
     Function_Number :     Pci_Function_Number
   ) return Process_Result is
      --  The results of internal PCI bus read processes.
      Result : x86.PCI.Process_Result;
      --  Array type to hold the data read from the PCI bus.
      type Bus_Output_Array is array (Unsigned_8 range 0 .. 31)
        of Unsigned_32;
      --  The data read from the PCI bus registers.
      Bus_Output : Bus_Output_Array;
   begin
      --  Read all of the PCI Device table into the bus ouput array.
      Read_Bus :
         begin
            for I in Bus_Output_Array'Range loop
               Result := x86.PCI.Pci_Read_Long (Bus_Output (I), Bus_Number,
                 Device_Number, Function_Number, I * 4);
               if Result /= Success then
                  return Bus_Read_Error;
               end if;
            end loop;
         end Read_Bus;

      Device.Bus_Number      := Bus_Number;
      Device.Device_Number   := Device_Number;
      Device.Function_Number := Function_Number;

      Device.Vendor_Id := Unsigned_16 (Bus_Output (0) and 16#FFFF#);
      Device.Device_Id := Unsigned_16 (Shift_Right (Bus_Output (0), 16));

      Device.Device_Class := Unsigned_8 (Shift_Right (Bus_Output (2), 24));
      Device.Subclass := Unsigned_8 (
        Shift_Right (Bus_Output (2), 16) and 16#FF#);
      Device.Prog_IF  := Unsigned_8 (
        Shift_Right (Bus_Output (2), 8) and 16#FF#);
      Device.Revision_Id := Unsigned_8 (Bus_Output (2) and 16#FF#);
      Device.BIST := Unsigned_8 (Shift_Right (Bus_Output (2), 24));

      Device.Header_Type := Unsigned_8 (
        Shift_Right (Bus_Output (3), 16) and 16#FF#);
      Device.Latency_Timer  := Unsigned_8 (
        Shift_Right (Bus_Output (3), 8) and 16#FF#);
      Device.Cache_Line_Size := Unsigned_8 (Bus_Output (3) and 16#FF#);
      Device.BAR0 := Bus_Output (5);

      return Success;
   exception
      when Constraint_Error =>
         return Bus_Read_Error;
   end Read_Pci_Device;

   ----------------------------------------------------------------------------
   --  Test_Pci_Device
   --
   --  Purpose:
   --    This function tests whether a device is present on the PCI bus at a
   --    particular address. The function sets a boolean value indicating
   --    whether a valid device is present.
   ----------------------------------------------------------------------------
   function Test_Pci_Device (
     Result          : out Boolean;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     Pci_Device_Number;
     Function_Number :     Pci_Function_Number
   ) return Process_Result is
      --  The result of the bus read process.
      Read_Result : x86.PCI.Process_Result;
      --  The long integer read from the bus during the test process.
      Output      : Unsigned_32;
   begin
      Read_Result := x86.PCI.Pci_Read_Long (Output, Bus_Number,
        Device_Number, Function_Number, 0);
      if Read_Result /= Success then
         return Bus_Read_Error;
      end if;

      if Output /= 16#FFFF_FFFF# then
         Result := True;
      else
         Result := False;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         Result := False;
         return Bus_Read_Error;
   end Test_Pci_Device;

end Cxos.PCI;
