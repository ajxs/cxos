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

package body Cxos.Devices.PCI.Print is
   use x86.PCI;
   package Chars renames Ada.Characters.Latin_1;
   procedure Debug_Print (Data : String) renames Cxos.Debug.Put_String;

   ----------------------------------------------------------------------------
   --  Print_Pci_Device
   ----------------------------------------------------------------------------
   procedure Print_Pci_Device (
      Device : Pci_Device
   ) is
   begin
      Debug_Print ("PCI Device:" & Chars.LF);
      Debug_Print ("  Bus:       " & Device.Bus_Number'Image & Chars.LF);
      Debug_Print ("  Device:    " & Device.Device_Number'Image & Chars.LF);
      Debug_Print ("  Function:  " & Device.Function_Number'Image & Chars.LF);
      Debug_Print ("  Vendor ID: " & Device.Vendor_Id'Image & Chars.LF);
      Debug_Print ("  Device ID: " & Device.Device_Id'Image & Chars.LF);
      Debug_Print ("  Class:     ");
      case Device.Device_Class is
         when 1 =>
            Debug_Print (" Mass Storage Controller" & Chars.LF);
            Debug_Print ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Debug_Print ("SCSI Bus Controller" & Chars.LF);
               when 1 =>
                  Debug_Print ("IDE Controller" & Chars.LF);
               when 2 =>
                  Debug_Print ("Floppy Disk Controller" & Chars.LF);
               when 3 =>
                  Debug_Print ("IPI Bus Controller" & Chars.LF);
               when 4 =>
                  Debug_Print ("RAID Controller" & Chars.LF);
               when 5 =>
                  Debug_Print ("ATA Controller" & Chars.LF);
               when 6 =>
                  Debug_Print ("Serial ATA" & Chars.LF);
               when 7 =>
                  Debug_Print ("Serial Attached SCSI" & Chars.LF);
               when 8 =>
                  Debug_Print ("Non-Volatile Memory Controller"
                    & Chars.LF);
               when 16#80# =>
                  Debug_Print ("Other" & Chars.LF);
               when others =>
                  Debug_Print ("Unknown: "
                    & Device.Subclass'Image & Chars.LF);
            end case;
         when 2 =>
            Debug_Print (" Network Controller" & Chars.LF);
            Debug_Print ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Debug_Print ("Ethernet Controller" & Chars.LF);
               when 1 =>
                  Debug_Print ("Token Ring Controller" & Chars.LF);
               when 2 =>
                  Debug_Print ("FDDI Controller" & Chars.LF);
               when 3 =>
                  Debug_Print ("ATM Controller" & Chars.LF);
               when 4 =>
                  Debug_Print ("ISDN Controller" & Chars.LF);
               when 5 =>
                  Debug_Print ("WorldFip Controller" & Chars.LF);
               when 6 =>
                  Debug_Print ("PICMG 2.14" & Chars.LF);
               when 7 =>
                  Debug_Print ("Infiniband Controller" & Chars.LF);
               when 8 =>
                  Debug_Print ("Fabric Controller" & Chars.LF);
               when 16#80# =>
                  Debug_Print ("Other" & Chars.LF);
               when others =>
                  Debug_Print ("Unknown: " & Device.Subclass'Image & Chars.LF);
            end case;
         when 3 =>
            Debug_Print (" VGA Controller" & Chars.LF);
            Debug_Print ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Debug_Print ("VGA Compatible" & Chars.LF);
               when 1 =>
                  Debug_Print ("XSA Controller" & Chars.LF);
               when 2 =>
                  Debug_Print ("3D Controller" & Chars.LF);
               when 16#80# =>
                  Debug_Print ("Other" & Chars.LF);
               when others =>
                  Debug_Print ("Unknown: "
                    & Device.Subclass'Image & Chars.LF);
            end case;
         when 6 =>
            Debug_Print (" Bridge Device" & Chars.LF);
            Debug_Print ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Debug_Print ("Host Bridge" & Chars.LF);
               when 1 =>
                  Debug_Print ("ISA Bridge" & Chars.LF);
               when 2 =>
                  Debug_Print ("EISA Bridge" & Chars.LF);
               when 3 =>
                  Debug_Print ("MCA Bridge" & Chars.LF);
               when 4 =>
                  Debug_Print ("PCI-to-PCI Bridge" & Chars.LF);
               when 5 =>
                  Debug_Print ("PCMCIA Bridge" & Chars.LF);
               when 6 =>
                  Debug_Print ("NuBus Bridge" & Chars.LF);
               when 7 =>
                  Debug_Print ("CardBus Bridge" & Chars.LF);
               when 8 =>
                  Debug_Print ("RACEway Bridge" & Chars.LF);
               when 9 =>
                  Debug_Print ("PCI-to-PCI Bridge" & Chars.LF);
               when 10 =>
                  Debug_Print ("InfiniBand Bridge" & Chars.LF);
               when 16#80# =>
                  Debug_Print ("Other" & Chars.LF);
               when others =>
                  Debug_Print ("Unknown: "
                    & Device.Subclass'Image & Chars.LF);
            end case;
         when 12 =>
            Debug_Print (" Serial Bus Controller" & Chars.LF);
            Debug_Print ("  Subclass:   ");
            case Device.Subclass is
               when 0 =>
                  Debug_Print ("Firewire Controller" & Chars.LF);
               when 1 =>
                  Debug_Print ("ACCESS Bus" & Chars.LF);
               when 2 =>
                  Debug_Print ("SSA" & Chars.LF);
               when 3 =>
                  Debug_Print ("USB Controller" & Chars.LF);
               when 4 =>
                  Debug_Print ("Fibre Channel" & Chars.LF);
               when 5 =>
                  Debug_Print ("SMBus" & Chars.LF);
               when 6 =>
                  Debug_Print ("InfiniBand" & Chars.LF);
               when 7 =>
                  Debug_Print ("IPMI Interface" & Chars.LF);
               when 8 =>
                  Debug_Print ("Sercos Interface" & Chars.LF);
               when 9 =>
                  Debug_Print ("CANbus" & Chars.LF);
               when 16#80# =>
                  Debug_Print ("Other" & Chars.LF);
               when others =>
                  Debug_Print ("Unknown: "
                    & Device.Subclass'Image & Chars.LF);
            end case;
         when others =>
            Debug_Print (Device.Device_Class'Image & Chars.LF);
            Debug_Print ("  Subclass:  "
              & Device.Subclass'Image & Chars.LF);
      end case;
      Debug_Print ("  Header:    "
        & Device.Header_Type'Image & Chars.LF);

      --  Print additional information based on header type.
      case (Device.Header_Type and 16#7F#) is
         when 0 =>
            Debug_Print ("  BAR0:      "
              & Device.BAR0'Image & Chars.LF);
            Debug_Print ("  BAR1:      "
              & Device.BAR1'Image & Chars.LF);
            Debug_Print ("  BAR2:      "
              & Device.BAR2'Image & Chars.LF);
            Debug_Print ("  BAR3:      "
              & Device.BAR3'Image & Chars.LF);
            Debug_Print ("  BAR4:      "
              & Device.BAR4'Image & Chars.LF);
            Debug_Print ("  BAR5:      "
              & Device.BAR5'Image & Chars.LF);
         when others =>
            null;
      end case;

      Debug_Print ("------------------------" & Chars.LF);
   exception
      when Constraint_Error =>
         Debug_Print ("Error printing device: " &
           "Invalid Value Encountered" & Chars.LF);
         return;
   end Print_Pci_Device;

end Cxos.Devices.PCI.Print;
