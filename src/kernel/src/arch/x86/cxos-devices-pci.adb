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
with System.Storage_Elements; use System.Storage_Elements;
with Cxos.Debug;
with Cxos.Devices.PCI.Print; use Cxos.Devices.PCI.Print;
with x86.Port_IO;

package body Cxos.Devices.PCI is
   package Chars renames Ada.Characters.Latin_1;
   procedure Debug_Print (Data : String) renames Cxos.Debug.Put_String;

   ----------------------------------------------------------------------------
   --  Image_Status
   ----------------------------------------------------------------------------
   function Image_Status (Status : Program_Status) return String is
   begin
      case Status is
         when Bus_Read_Error =>
            return "Bus read error";
         when Invalid_Argument =>
            return "Invalid argument";
         when Misaligned_Offset =>
            return "Misaligned offset";
         when Success =>
            return "Success";
         when Unhandled_Exception =>
            return "Unhandled Exception";
      end case;
   exception
      when Constraint_Error =>
         return "Unknown";
   end Image_Status;

   ----------------------------------------------------------------------------
   --  Query_PCI_Bus
   ----------------------------------------------------------------------------
   procedure Query_PCI_Bus (
     PCI_Bus : out Device_Bus_T;
     Status  : out Program_Status
   ) is
      --  Variable to store the last read device.
      Device_Info : Device_T;
      --  Variable for testing whether a device exists at a specific address.
      Test_Result : Boolean;
      --  Whether debug device info should be printed to serial out.
      PRINT_INFO  : constant Boolean := True;
   begin
      PCI_Bus := (
         Device_Bus_Type => Device_Bus_Type_PCI
      );

      Debug_Print ("Testing PCI Bus" & Chars.LF &
        "------------------------" & Chars.LF);

      for Bus in Unsigned_8 range 0 .. 255 loop
         for Device in x86.PCI.PCI_Device_Number range 0 .. 31 loop
            Function_Loop :
               for Func in PCI_Function_Number range 0 .. 7 loop
                  --  Test the individual PCI address.
                  Test_PCI_Device (Test_Result, Bus, Device, Func, Status);
                  if Status /= Success then
                     Debug_Print ("Error testing PCI device: " &
                       Image_Status (Status) & Chars.LF);

                     return;
                  end if;

                  if Test_Result then
                     Read_PCI_Device (Device_Info, Bus, Device, Func, Status);
                     if Status /= Success then
                        Debug_Print ("Error reading PCI device: " &
                          Image_Status (Status) & Chars.LF);

                        return;
                     end if;

                     if PRINT_INFO then
                        Print_PCI_Device (Device_Info);
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

      Status := Success;
   exception
      when Constraint_Error =>
         Status := Unhandled_Exception;
   end Query_PCI_Bus;

   ----------------------------------------------------------------------------
   --  Read_Long
   ----------------------------------------------------------------------------
   procedure Read_Long (
     Output          : out Unsigned_32;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     PCI_Device_Number;
     Function_Number :     PCI_Function_Number;
     Offset          :     Unsigned_8;
     Status          : out Program_Status
   ) is
      --  The PCI address register value to send to read
      --  the value back from.
      Register_Address : Pci_Config_Address;
   begin
      --  Offset addresses consecutive DWORDs in the PCI addres space, as such
      --  it must be a multiple of 4.
      if (Offset and 3) /= 0 then
         Status := Misaligned_Offset;
         Output := 0;

         return;
      end if;

      --  Configure the register address to read.
      Register_Address := (
        Offset          => Offset,
        Function_Number => Function_Number,
        Device_Number   => Device_Number,
        Bus_Number      => Bus_Number,
        Reserved        => False,
        Enable          => True
      );

      --  Set the address register.
      x86.Port_IO.Outl (To_Address (PCI_CONFIG_ADDRESS_PORT),
        Pci_Config_Address_To_Long (Register_Address));
      --  Read the data in.
      Output := x86.Port_IO.Inl (To_Address (PCI_CONFIG_DATA_PORT));

      Status := Success;
   end Read_Long;

   ----------------------------------------------------------------------------
   --  Read_Pci_Device
   ----------------------------------------------------------------------------
   procedure Read_PCI_Device (
     Device          : out Device_T;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     PCI_Device_Number;
     Function_Number :     PCI_Function_Number;
     Status          : out Program_Status
   ) is
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
               Read_Long (Bus_Output (I), Bus_Number,
                 Device_Number, Function_Number, I * 4, Status);
               if Status /= Success then
                  return;
               end if;
            end loop;
         end Read_Bus;

      --  Device.Device_Type     := Device_Type_PCI;
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

      Status := Success;
   exception
      when Constraint_Error =>
         Status := Unhandled_Exception;
   end Read_PCI_Device;

   ----------------------------------------------------------------------------
   --  Test_Pci_Device
   --
   --  Implementation Notes:
   --    - Reads the Vendor ID field of the PCI Device at the specified
   --      address. If a null value of 0xFFFFFFFF is returned, we can
   --      conclude that no device exists at this bus address.
   ----------------------------------------------------------------------------
   procedure Test_PCI_Device (
     Result          : out Boolean;
     Bus_Number      :     Unsigned_8;
     Device_Number   :     PCI_Device_Number;
     Function_Number :     PCI_Function_Number;
     Status          : out Program_Status
   ) is
      --  The long integer read from the bus during the test process.
      Output      : Unsigned_32;
   begin
      --  Read the first DWORD from the PCI Bus.
      Read_Bus :
         begin
            Read_Long (Output, Bus_Number, Device_Number,
              Function_Number, 0, Status);
            if Status /= Success then
               Result := False;

               return;
            end if;
         exception
            when Constraint_Error =>
               Status := Unhandled_Exception;

               return;
         end Read_Bus;

      --  Test whether reading the first DWORD of the device at this address
      --  returns a null value of 0xFFFF_FFFF.
      Result := Output /= 16#FFFF_FFFF#;

      Status := Success;
   end Test_PCI_Device;

end Cxos.Devices.PCI;
