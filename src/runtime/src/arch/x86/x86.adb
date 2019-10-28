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

with Ada.Interrupts.Names;
with x86.Exceptions;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with x86.IDT;
with x86.Interrupts;
with x86.IRQ_Handlers;
with x86.GDT;
with x86.PIC;
with x86.Memory.Paging;
with x86.Memory.Map;
with x86.PIT;
with x86.Serial;
with x86.Time_Keeping;
with x86.Vga;

package body x86 is
   use Ada.Interrupts.Names;
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise (
     Magic_Number      : Multiboot_Magic_Number;
     Boot_Info_Address : System.Address
   ) is
      --  Create multiboot info structure overlaid at boot info address.
      Boot_Info : constant Multiboot_Info
      with Address => Boot_Info_Address,
        Import,
        Convention => C,
        Volatile;

      Multiboot_Memory_Map_Present : Boolean;
   begin
      x86.Vga.Clear (x86.Vga.Black);
      x86.Vga.Put_String (0, 0, x86.Vga.Light_Green, x86.Vga.Black,
        "VGA Text Mode Initialised");

      x86.Serial.Initialise (x86.Serial.COM1, 38400);
      x86.Serial.Put_String (x86.Serial.COM1,
        "COM1 initialised" & ASCII.LF);

      --  Check whether we were booted by a Multiboot compatible bootloader.
      if Magic_Number = VALID_MAGIC_NUMBER then
         x86.Serial.Put_String (x86.Serial.COM1,
           "Detected valid Multiboot magic number" & ASCII.LF);

         Detect_Memory_Map :
            begin
               if Boot_Info.Flags.Memory_Map_Fields_Valid then
                  x86.Serial.Put_String (x86.Serial.COM1,
                     "Multiboot memory map present" & ASCII.LF);

                  Multiboot_Memory_Map_Present := True;
               else
                  x86.Serial.Put_String (x86.Serial.COM1,
                     "Multiboot memory map not present" & ASCII.LF);

                  Multiboot_Memory_Map_Present := False;
               end if;
            exception
               when Constraint_Error =>
                  x86.Serial.Put_String (x86.Serial.COM1,
                     "Error detecting Multiboot memory map" & ASCII.LF);

                  Multiboot_Memory_Map_Present := False;
            end Detect_Memory_Map;
      else
         x86.Serial.Put_String (x86.Serial.COM1,
           "Unable to detect valid Multiboot magic number" & ASCII.LF);
      end if;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising PIC" & ASCII.LF);
      x86.PIC.Initialise;

      --  Clear interrupts.
      x86.Interrupts.Set_Interrupt_Flag (False);

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising GDT" & ASCII.LF);
      x86.GDT.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising IDT" & ASCII.LF);
      x86.IDT.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Installing processor exception handlers" & ASCII.LF);
      Install_Exception_Handlers;

      --  Install a handler for IRQ0.
      x86.PIC.Set_Interrupt_Mask (IRQ0, False);
      x86.IDT.Install_Descriptor (32,
        x86.IRQ_Handlers.IRQ0_Handler'Address, 16#8#);

      --  Install a handler for IRQ1.
      x86.PIC.Set_Interrupt_Mask (IRQ1, False);
      x86.IDT.Install_Descriptor (33,
        x86.IRQ_Handlers.IRQ1_Handler'Address, 16#8#);

      x86.IDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Flushing GDT" & ASCII.LF);
      x86.GDT.Finalise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Jumping to protected mode" & ASCII.LF);
      Protected_Mode_Init;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising system timer" & ASCII.LF);
      x86.Time_Keeping.Initialise;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising PIT" & ASCII.LF);
      x86.PIT.Initialise;

      Initialise_Memory_Map :
         begin
            x86.Serial.Put_String (x86.Serial.COM1,
              "Initialising Memory Map" & ASCII.LF);
            x86.Memory.Map.Initialise;

            if Multiboot_Memory_Map_Present then
               x86.Serial.Put_String (x86.Serial.COM1,
                 "Parsing Multiboot memory map" & ASCII.LF);

               Parse_Multiboot_Memory_Map (
                 To_Address (Integer_Address (Boot_Info.Mmap_Addr)),
                 Boot_Info.Mmap_Length);
            end if;

            --  Mark memory below 1MB as used.
            Mark_Low_Memory;
            --  Mark kernel code segment as being used.
            Mark_Kernel_Memory;
         exception
            when Constraint_Error =>
               x86.Serial.Put_String (x86.Serial.COM1,
                 "Error parsing Multiboot memory map" & ASCII.LF);
         end Initialise_Memory_Map;

      --  Enable interrupts.
      x86.Interrupts.Set_Interrupt_Flag (True);

      x86.Serial.Put_String (x86.Serial.COM1,
        "Protected mode entered" & ASCII.LF);

      x86.Serial.Put_String (x86.Serial.COM1,
        "Initialising Kernel Memory Map" & ASCII.LF);
      x86.Memory.Paging.Map_Kernel;

      x86.Memory.Paging.Initialise_Kernel_Page_Directory;

      x86.Serial.Put_String (x86.Serial.COM1,
        "Enabling Paging" & ASCII.LF);
      x86.Memory.Paging.Enable_Paging;
      x86.Serial.Put_String (x86.Serial.COM1,
        "Paging Enabled" & ASCII.LF);
   end Initialise;

   ----------------------------------------------------------------------------
   --  Install_Exception_Handlers
   ----------------------------------------------------------------------------
   procedure Install_Exception_Handlers is
   begin
      x86.IDT.Install_Descriptor (0,
        x86.Exceptions.Exception_0_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (1,
        x86.Exceptions.Exception_1_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (2,
        x86.Exceptions.Exception_2_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (3,
        x86.Exceptions.Exception_3_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (4,
        x86.Exceptions.Exception_4_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (5,
        x86.Exceptions.Exception_5_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (6,
        x86.Exceptions.Exception_6_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (7,
        x86.Exceptions.Exception_7_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (8,
        x86.Exceptions.Exception_8_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (9,
        x86.Exceptions.Exception_9_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (10,
        x86.Exceptions.Exception_10_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (11,
        x86.Exceptions.Exception_11_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (12,
        x86.Exceptions.Exception_12_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (13,
        x86.Exceptions.Exception_13_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (14,
        x86.Exceptions.Exception_14_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (15,
        x86.Exceptions.Exception_15_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (16,
        x86.Exceptions.Exception_16_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (17,
        x86.Exceptions.Exception_17_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (18,
        x86.Exceptions.Exception_18_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (19,
        x86.Exceptions.Exception_19_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (20,
        x86.Exceptions.Exception_20_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (21,
        x86.Exceptions.Exception_21_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (22,
        x86.Exceptions.Exception_22_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (23,
        x86.Exceptions.Exception_23_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (24,
        x86.Exceptions.Exception_24_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (25,
        x86.Exceptions.Exception_25_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (26,
        x86.Exceptions.Exception_26_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (27,
        x86.Exceptions.Exception_27_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (28,
        x86.Exceptions.Exception_28_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (29,
        x86.Exceptions.Exception_29_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (30,
        x86.Exceptions.Exception_30_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (31,
        x86.Exceptions.Exception_31_Handler'Address, 16#8#);

   end Install_Exception_Handlers;

   ----------------------------------------------------------------------------
   --  Last_Chance_Handler
   ----------------------------------------------------------------------------
   procedure Last_Chance_Handler (
     Msg  : System.Address;
     Line : Integer
   ) is
   begin
      null;
   end Last_Chance_Handler;

   ----------------------------------------------------------------------------
   --  Mark_Kernel_Memory
   ----------------------------------------------------------------------------
   procedure Mark_Kernel_Memory is
      use x86.Memory.Map;

      --  The length of the kernel code segment in bytes.
      Kernel_Length    : Unsigned_32;
      --  The result of the frame status set process.
      Result : x86.Memory.Map.Process_Result;
      --  The start of the kernel code segment.
      Kernel_Start     : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_Name => "kernel_start";
      --  The end of the kernel code segment.
      Kernel_End       : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_Name => "kernel_end";
   begin
      Kernel_Length   := Unsigned_32 (
        To_Integer (Kernel_End'Address) -
        To_Integer (Kernel_Start'Address));

      Result := x86.Memory.Map.Mark_Memory_Range (
        Kernel_Start'Address, Kernel_Length, False);

      if Result /= Success then
         x86.Serial.Put_String (x86.Serial.COM1,
           "Error marking kernel code segment" & ASCII.LF);
      end if;

   exception
      when Constraint_Error =>
         return;
   end Mark_Kernel_Memory;

   ----------------------------------------------------------------------------
   --  Mark_Low_Memory
   ----------------------------------------------------------------------------
   procedure Mark_Low_Memory is
      use x86.Memory.Map;

      --  The result of the process.
      Result : x86.Memory.Map.Process_Result;
   begin
      Result := x86.Memory.Map.Mark_Memory_Range (To_Address (0),
        16#100000#, False);

      if Result /= Success then
         x86.Serial.Put_String (x86.Serial.COM1,
           "Error setting memory range" & ASCII.LF);
         return;
      end if;
   exception
      when Constraint_Error =>
         return;
   end Mark_Low_Memory;

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Memory_Map
   ----------------------------------------------------------------------------
   procedure Parse_Multiboot_Memory_Map (
     Memory_Map_Addr   : System.Address;
     Memory_Map_Length : Unsigned_32
   ) is
      package Mmap_Region_Ptr is new
        System.Address_To_Access_Conversions (Multiboot.Multiboot_Mmap_Region);

      --  The total number of bytes read in the Multiboot mmap region.
      Bytes_Read  : Unsigned_32    := 0;
      --  The address of the current mmap region structure.
      Curr_Addr   : System.Address := Memory_Map_Addr;
      --  A pointer to the current mmap region structure.
      Curr_Region : Mmap_Region_Ptr.Object_Pointer :=
        Mmap_Region_Ptr.To_Pointer (Curr_Addr);
   begin
      while Bytes_Read < Memory_Map_Length loop
         --  Reset the current region pointer.
         Curr_Region := Mmap_Region_Ptr.To_Pointer (Curr_Addr);

         --  Print information about the current mmap region.
         Print_Memory_Region_Info :
            begin
               x86.Serial.Put_String (x86.Serial.COM1,
                 "Parsing Mmap region" & ASCII.LF);
               x86.Serial.Put_String (x86.Serial.COM1, "  Type:  ");

               case Curr_Region.all.Memory_Type is
                  when 1 =>
                     x86.Serial.Put_String (x86.Serial.COM1,
                       "Free RAM" & ASCII.LF);
                  when 3 =>
                     x86.Serial.Put_String (x86.Serial.COM1,
                       "ACPI" & ASCII.LF);
                  when 4 =>
                     x86.Serial.Put_String (x86.Serial.COM1,
                       "Reserved for hibernation" & ASCII.LF);
                  when 5 =>
                     x86.Serial.Put_String (x86.Serial.COM1,
                       "Defective" & ASCII.LF);
                  when others =>
                     x86.Serial.Put_String (x86.Serial.COM1,
                       "Reserved" & ASCII.LF);
               end case;

               x86.Serial.Put_String (x86.Serial.COM1,
                 "  Base:  __" & ASCII.LF);
               x86.Serial.Put_String (x86.Serial.COM1,
                 "  Limit: __" & ASCII.LF);
            exception
               when Constraint_Error =>
                  return;
            end Print_Memory_Region_Info;

         --  Mark free memory in the kernel memory map.
         Mark_Free_Memory :
            declare
               use x86.Memory.Map;

               --  The result of the frame status set process.
               Result : x86.Memory.Map.Process_Result;
            begin
               --  If the memory region is marked as free, set the status
               --  accordingly in the memory map.
               case Curr_Region.all.Memory_Type is
                  when 1 =>
                     Result := x86.Memory.Map.Mark_Memory_Range (
                       To_Address (Integer_Address (Curr_Region.all.Base)),
                       Unsigned_32 (Curr_Region.all.Length), True);
                     if Result /= Success then
                        x86.Serial.Put_String (x86.Serial.COM1,
                          "Error setting frame status" & ASCII.LF);
                        return;
                     end if;
                  when others =>
                     null;
               end case;
            exception
               when Constraint_Error =>
                  x86.Serial.Put_String (x86.Serial.COM1,
                    "Error marking free memory" & ASCII.LF);
                  return;
            end Mark_Free_Memory;
         Increment_Pointer :
            begin
               --  The 'Size' value is not inclusive of the size variable
               --  itself. It refers to the size of the internal structure.
               Curr_Addr := To_Address (To_Integer (Curr_Addr) +
                 Integer_Address (4 + Curr_Region.all.Size));

               Bytes_Read := Bytes_Read + 4 + Curr_Region.all.Size;
            exception
               when Constraint_Error =>
                  return;
            end Increment_Pointer;
      end loop;
   end Parse_Multiboot_Memory_Map;

end x86;
