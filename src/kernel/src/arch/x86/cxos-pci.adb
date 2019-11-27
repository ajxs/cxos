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
                  Cxos.Serial.Put_String ("------------------------" &
                    ASCII.LF);
                  Cxos.Serial.Put_String ("Bus:      " & Bus'Image & ASCII.LF);
                  Cxos.Serial.Put_String ("Device:   " & Device'Image
                    & ASCII.LF);
                  Cxos.Serial.Put_String ("Function:  0" & ASCII.LF);

                  Print_Pci_Device (Device_Info);
               end if;

               --  If this is a multi-function device.
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
                           Cxos.Serial.Put_String ("------------------------" &
                             ASCII.LF);
                           Cxos.Serial.Put_String ("Bus:      " &
                             Bus'Image & ASCII.LF);
                           Cxos.Serial.Put_String ("Device:   " &
                             Device'Image & ASCII.LF);
                           Cxos.Serial.Put_String ("Function: " &
                             Func'Image & ASCII.LF);

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
      Cxos.Serial.Put_String ("Device:" & ASCII.LF);

      Cxos.Serial.Put_String ("  Vendor ID: "
        & Device.Vendor_Id'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  Device ID: "
        & Device.Device_Id'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  Class:     "
        & Device.Device_Class'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  Header:    "
        & Device.Header_Type'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  Subclass:  "
        & Device.Subclass'Image & ASCII.LF);
      Cxos.Serial.Put_String ("  BAR0:      "
        & Device.BAR0'Image & ASCII.LF);

   end Print_Pci_Device;

   ----------------------------------------------------------------------------
   --  Read_Pci_Device
   ----------------------------------------------------------------------------
   function Read_Pci_Device (
     Output          : out Pci_Device;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     Pci_Device_Number;
     Function_Number :     Pci_Function_Number
   ) return Process_Result is
      --  The results of internal PCI bus read processes.
      Result : x86.PCI.Process_Result;
      --  The value read from the PCI bus registers.
      Bus_Output : Unsigned_32;
   begin
      Result := x86.PCI.Pci_Read_Long (Bus_Output, Bus_Number,
        Device_Number, Function_Number, 0);
      if Result /= Success then
         return Bus_Read_Error;
      end if;

      Output.Vendor_Id := Unsigned_16 (Bus_Output and 16#FFFF#);
      Output.Device_Id := Unsigned_16 (Shift_Right (Bus_Output, 16));

      Result := x86.PCI.Pci_Read_Long (Bus_Output, Bus_Number,
        Device_Number, Function_Number, 8);
      if Result /= Success then
         return Bus_Read_Error;
      end if;

      Output.Device_Class := Unsigned_8 (Shift_Right (Bus_Output, 24));
      Output.Subclass := Unsigned_8 (Shift_Right (Bus_Output, 16) and 16#FF#);
      Output.Prog_IF  := Unsigned_8 (Shift_Right (Bus_Output, 8) and 16#FF#);
      Output.Revision_Id := Unsigned_8 (Bus_Output and 16#FF#);

      Result := x86.PCI.Pci_Read_Long (Bus_Output, Bus_Number,
        Device_Number, Function_Number, 16#C#);
      if Result /= Success then
         return Bus_Read_Error;
      end if;

      Output.BIST := Unsigned_8 (Shift_Right (Bus_Output, 24));
      Output.Header_Type := Unsigned_8 (
        Shift_Right (Bus_Output, 16) and 16#FF#);
      Output.Latency_Timer  := Unsigned_8 (
        Shift_Right (Bus_Output, 8) and 16#FF#);
      Output.Cache_Line_Size := Unsigned_8 (Bus_Output and 16#FF#);

      Result := x86.PCI.Pci_Read_Long (Bus_Output, Bus_Number,
        Device_Number, Function_Number, 16#10#);
      if Result /= Success then
         return Bus_Read_Error;
      end if;

      Output.BAR0 := Bus_Output;

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
      Read_Result : x86.PCI.Process_Result;
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
