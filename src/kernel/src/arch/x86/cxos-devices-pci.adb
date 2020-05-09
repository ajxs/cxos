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

package body Cxos.Devices.PCI is
   use x86.PCI;
   package Chars renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------------
   --  Find_Pci_Devices
   ----------------------------------------------------------------------------
   function Find_Pci_Devices return Process_Result is
      --  Variable to store the last read device.
      Device_Info : Pci_Device;
      --  Variable for testing whether a device exists at a specific address.
      Test_Result : Boolean;
      --  The result of internal processes.
      Result      : Process_Result;
      --  Whether debug device info should be printed to serial out.
      PRINT_INFO  : constant Boolean := True;
   begin
      Cxos.Debug.Put_String ("Testing PCI Bus" & Chars.LF &
        "------------------------" & Chars.LF);

      for Bus in Unsigned_8 range 0 .. 255 loop
         for Device in x86.PCI.Pci_Device_Number range 0 .. 31 loop
            Function_Loop :
               for Func in Pci_Function_Number range 0 .. 7 loop
                  --  Test the individual PCI address.
                  Result := Test_Pci_Device (Test_Result, Bus, Device, Func);
                  if Result /= Success then
                     Cxos.Debug.Put_String ("Error testing PCI device"
                       & Chars.LF);
                     return Unhandled_Exception;
                  end if;

                  if Test_Result then
                     Result := Read_Pci_Device (Device_Info, Bus,
                       Device, Func);
                     if Result /= Success then
                        Cxos.Debug.Put_String ("Error reading PCI device" &
                          Chars.LF);
                        return Unhandled_Exception;
                     end if;

                     if PRINT_INFO then
                        Print_Pci_Device (Device_Info);
                     end if;

                     --  If this is not a multi-function device, exit.
                     if Func = 0 and (Device_Info.Header_Type and 16#80#) = 0
                     then
                        exit Function_Loop;
                     end if;

                  end if;
               end loop Function_Loop;
         end loop;
      end loop;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Find_Pci_Devices;

   ----------------------------------------------------------------------------
   --  Print_Pci_Device
   ----------------------------------------------------------------------------
   procedure Print_Pci_Device (
      Device : Pci_Device
   ) is
   begin
      Cxos.Debug.Put_String ("Device:" & Chars.LF);
      Cxos.Debug.Put_String ("  Bus:       "
        & Device.Bus_Number'Image & Chars.LF);
      Cxos.Debug.Put_String ("  Device:    "
        & Device.Device_Number'Image & Chars.LF);
      Cxos.Debug.Put_String ("  Function:  "
        & Device.Function_Number'Image & Chars.LF);
      Cxos.Debug.Put_String ("  Vendor ID: "
        & Device.Vendor_Id'Image & Chars.LF);
      Cxos.Debug.Put_String ("  Device ID: "
        & Device.Device_Id'Image & Chars.LF);
      Cxos.Debug.Put_String ("  Class:     ");
      case Device.Device_Class is
         when 1 =>
            Cxos.Debug.Put_String (" Mass Storage Controller" & Chars.LF);
            Cxos.Debug.Put_String ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Cxos.Debug.Put_String ("SCSI Bus Controller" & Chars.LF);
               when 1 =>
                  Cxos.Debug.Put_String ("IDE Controller" & Chars.LF);
               when 2 =>
                  Cxos.Debug.Put_String ("Floppy Disk Controller" & Chars.LF);
               when 3 =>
                  Cxos.Debug.Put_String ("IPI Bus Controller" & Chars.LF);
               when 4 =>
                  Cxos.Debug.Put_String ("RAID Controller" & Chars.LF);
               when 5 =>
                  Cxos.Debug.Put_String ("ATA Controller" & Chars.LF);
               when 6 =>
                  Cxos.Debug.Put_String ("Serial ATA" & Chars.LF);
               when 7 =>
                  Cxos.Debug.Put_String ("Serial Attached SCSI" & Chars.LF);
               when 8 =>
                  Cxos.Debug.Put_String ("Non-Volatile Memory Controller"
                    & Chars.LF);
               when 16#80# =>
                  Cxos.Debug.Put_String ("Other" & Chars.LF);
               when others =>
                  Cxos.Debug.Put_String ("Unknown: "
                    & Device.Subclass'Image & Chars.LF);
            end case;
         when 2 =>
            Cxos.Debug.Put_String (" Network Controller" & Chars.LF);
            Cxos.Debug.Put_String ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Cxos.Debug.Put_String ("Ethernet Controller" & Chars.LF);
               when 1 =>
                  Cxos.Debug.Put_String ("Token Ring Controller" & Chars.LF);
               when 2 =>
                  Cxos.Debug.Put_String ("FDDI Controller" & Chars.LF);
               when 3 =>
                  Cxos.Debug.Put_String ("ATM Controller" & Chars.LF);
               when 4 =>
                  Cxos.Debug.Put_String ("ISDN Controller" & Chars.LF);
               when 5 =>
                  Cxos.Debug.Put_String ("WorldFip Controller" & Chars.LF);
               when 6 =>
                  Cxos.Debug.Put_String ("PICMG 2.14" & Chars.LF);
               when 7 =>
                  Cxos.Debug.Put_String ("Infiniband Controller" & Chars.LF);
               when 8 =>
                  Cxos.Debug.Put_String ("Fabric Controller" & Chars.LF);
               when 16#80# =>
                  Cxos.Debug.Put_String ("Other" & Chars.LF);
               when others =>
                  Cxos.Debug.Put_String ("Unknown: "
                    & Device.Subclass'Image & Chars.LF);
            end case;
         when 3 =>
            Cxos.Debug.Put_String (" VGA Controller" & Chars.LF);
            Cxos.Debug.Put_String ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Cxos.Debug.Put_String ("VGA Compatible" & Chars.LF);
               when 1 =>
                  Cxos.Debug.Put_String ("XSA Controller" & Chars.LF);
               when 2 =>
                  Cxos.Debug.Put_String ("3D Controller" & Chars.LF);
               when 16#80# =>
                  Cxos.Debug.Put_String ("Other" & Chars.LF);
               when others =>
                  Cxos.Debug.Put_String ("Unknown: "
                    & Device.Subclass'Image & Chars.LF);
            end case;
         when 6 =>
            Cxos.Debug.Put_String (" Bridge Device" & Chars.LF);
            Cxos.Debug.Put_String ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Cxos.Debug.Put_String ("Host Bridge" & Chars.LF);
               when 1 =>
                  Cxos.Debug.Put_String ("ISA Bridge" & Chars.LF);
               when 2 =>
                  Cxos.Debug.Put_String ("EISA Bridge" & Chars.LF);
               when 3 =>
                  Cxos.Debug.Put_String ("MCA Bridge" & Chars.LF);
               when 4 =>
                  Cxos.Debug.Put_String ("PCI-to-PCI Bridge" & Chars.LF);
               when 5 =>
                  Cxos.Debug.Put_String ("PCMCIA Bridge" & Chars.LF);
               when 6 =>
                  Cxos.Debug.Put_String ("NuBus Bridge" & Chars.LF);
               when 7 =>
                  Cxos.Debug.Put_String ("CardBus Bridge" & Chars.LF);
               when 8 =>
                  Cxos.Debug.Put_String ("RACEway Bridge" & Chars.LF);
               when 9 =>
                  Cxos.Debug.Put_String ("PCI-to-PCI Bridge" & Chars.LF);
               when 10 =>
                  Cxos.Debug.Put_String ("InfiniBand Bridge" & Chars.LF);
               when 16#80# =>
                  Cxos.Debug.Put_String ("Other" & Chars.LF);
               when others =>
                  Cxos.Debug.Put_String ("Unknown: "
                    & Device.Subclass'Image & Chars.LF);
            end case;
         when 12 =>
            Cxos.Debug.Put_String (" Serial Bus Controller" & Chars.LF);
            Cxos.Debug.Put_String ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Cxos.Debug.Put_String ("Firewire Controller" & Chars.LF);
               when 1 =>
                  Cxos.Debug.Put_String ("ACCESS Bus" & Chars.LF);
               when 2 =>
                  Cxos.Debug.Put_String ("SSA" & Chars.LF);
               when 3 =>
                  Cxos.Debug.Put_String ("USB Controller" & Chars.LF);
               when 4 =>
                  Cxos.Debug.Put_String ("Fibre Channel" & Chars.LF);
               when 5 =>
                  Cxos.Debug.Put_String ("SMBus" & Chars.LF);
               when 6 =>
                  Cxos.Debug.Put_String ("InfiniBand" & Chars.LF);
               when 7 =>
                  Cxos.Debug.Put_String ("IPMI Interface" & Chars.LF);
               when 8 =>
                  Cxos.Debug.Put_String ("Sercos Interface" & Chars.LF);
               when 9 =>
                  Cxos.Debug.Put_String ("CANbus" & Chars.LF);
               when 16#80# =>
                  Cxos.Debug.Put_String ("Other" & Chars.LF);
               when others =>
                  Cxos.Debug.Put_String ("Unknown: "
                    & Device.Subclass'Image & Chars.LF);
            end case;
         when others =>
            Cxos.Debug.Put_String (Device.Device_Class'Image & Chars.LF);
            Cxos.Debug.Put_String ("  Subclass:  "
              & Device.Subclass'Image & Chars.LF);
      end case;
      Cxos.Debug.Put_String ("  Header:    "
        & Device.Header_Type'Image & Chars.LF);

      --  Print additional information based on header type.
      case (Device.Header_Type and 16#7F#) is
         when 0 =>
            Cxos.Debug.Put_String ("  BAR0:      "
              & Device.BAR0'Image & Chars.LF);
            Cxos.Debug.Put_String ("  BAR1:      "
              & Device.BAR1'Image & Chars.LF);
            Cxos.Debug.Put_String ("  BAR2:      "
              & Device.BAR2'Image & Chars.LF);
            Cxos.Debug.Put_String ("  BAR3:      "
              & Device.BAR3'Image & Chars.LF);
            Cxos.Debug.Put_String ("  BAR4:      "
              & Device.BAR4'Image & Chars.LF);
            Cxos.Debug.Put_String ("  BAR5:      "
              & Device.BAR5'Image & Chars.LF);
         when others =>
            null;
      end case;

      Cxos.Debug.Put_String ("------------------------" & Chars.LF);
   exception
      when Constraint_Error =>
         Cxos.Debug.Put_String ("Error printing device: " &
           "Invalid Value Encountered" & Chars.LF);
         return;
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

      --  Print additional information based on header type.
      case (Device.Header_Type and 16#7F#) is
         when 0 =>
            Device.BAR0 := Bus_Output (5);
            Device.BAR1 := Bus_Output (6);
            Device.BAR2 := Bus_Output (7);
            Device.BAR3 := Bus_Output (8);
            Device.BAR4 := Bus_Output (9);
         when others =>
            null;
      end case;

      return Success;
   exception
      when Constraint_Error =>
         return Bus_Read_Error;
   end Read_Pci_Device;

   ----------------------------------------------------------------------------
   --  Test_Pci_Device
   --
   --  Implementation Notes:
   --    - Reads the Vendor ID field of the PCI Device at the specified
   --      address. If a null value of 0xFFFFFFFF is returned, we can
   --      conclude that no device exists at this bus address.
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
      --  Read the first DWORD from the PCI Bus.
      Read_Bus :
         begin
            Read_Result := x86.PCI.Pci_Read_Long (Output, Bus_Number,
              Device_Number, Function_Number, 0);
            if Read_Result /= Success then
               return Bus_Read_Error;
            end if;
         exception
            when Constraint_Error =>
               return Bus_Read_Error;
         end Read_Bus;

      --  Test whether reading the first DWORD of the device at this address
      --  returns a null value of 0xFFFF_FFFF.
      Result := Output /= 16#FFFF_FFFF#;

      return Success;
   end Test_Pci_Device;

end Cxos.Devices.PCI;
