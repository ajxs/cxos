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
with Multiboot;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with x86.Memory.Map;
with x86.Memory.Paging;

package body Cxos.Memory is
   ----------------------------------------------------------------------------
   --  Clear_Boot_Page_Structures
   ----------------------------------------------------------------------------
   function Clear_Boot_Page_Structures return Kernel_Process_Result is
      use x86.Memory.Map;

      --  The result of the frame status set process.
      Result : x86.Memory.Map.Process_Result;

      --  The boot page directory.
      --  Import as an Unsigned int, since we don't care what kind of
      --  structure is at this memory address. We only need to clear it.
      Boot_Page_Directory : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_Name => "boot_page_directory";

      --  The boot page table.
      Boot_Page_Table     : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_Name => "boot_page_table";
   begin
      Result := x86.Memory.Map.Mark_Memory_Range (
        Boot_Page_Directory'Address, 16#1000#, Unallocated);
      if Result /= Success then
         Cxos.Serial.Put_String (
           "Error freeing boot page directory" & ASCII.LF);
         return Failure;
      end if;

      Result := x86.Memory.Map.Mark_Memory_Range (
        Boot_Page_Table'Address, 16#1000#, Unallocated);
      if Result /= Success then
         Cxos.Serial.Put_String (
           "Error freeing boot page table" & ASCII.LF);
         return Failure;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Failure;
   end Clear_Boot_Page_Structures;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   function Initialise return Kernel_Process_Result is
      use Multiboot;
      use System.Storage_Elements;

      --  Multiboot info struct address.
      Boot_Info_Address : constant System.Address
      with Import,
        Convention    => Assembler,
        External_Name => "multiboot_struct",
        Volatile;
      --  Create multiboot info structure overlaid at boot info address.
      Boot_Info         : constant Multiboot_Info
      with Import,
        Convention    => Assembler,
        Address       => Boot_Info_Address,
        Volatile;

      --  The result of the internal processes.
      Init_Result : Cxos.Kernel_Process_Result;
   begin
      --  Check whether we have a valid Multiboot memory map.
      if Boot_Info.Flags.Memory_Map_Fields_Valid then
         Cxos.Serial.Put_String (
           "Multiboot memory map present" & ASCII.LF &
           "Parsing Multiboot memory map" & ASCII.LF);

         --  Parse the Multiboot provided memory map to mark memory
         --  regions that are free to use.
         Parse_Multiboot_Memory_Map (
           To_Address (Integer_Address (Boot_Info.Mmap_Addr)),
           Boot_Info.Mmap_Length);

         --  Mark used memory.
         Map_System_Memory :
            begin
               --  Mark memory below 1MB as used.
               Cxos.Serial.Put_String ("Marking low memory" & ASCII.LF);
               Init_Result := Mark_Low_Memory;
               if Init_Result /= Success then
                  Cxos.Serial.Put_String (
                    "Error marking low memory" & ASCII.LF);
                  return Init_Result;
               end if;

               --  Mark kernel segments as being used.
               Cxos.Serial.Put_String ("Marking kernel memory" & ASCII.LF);
               Init_Result := Mark_Kernel_Memory;
               if Init_Result /= Success then
                  Cxos.Serial.Put_String (
                    "Error marking kernel memory" & ASCII.LF);
                  return Init_Result;
               end if;
            exception
               when Constraint_Error =>
                  Cxos.Serial.Put_String (
                    "Error marking used memory" & ASCII.LF);
                  return Failure;
            end Map_System_Memory;
      else
         Cxos.Serial.Put_String (
           "Multiboot memory map not present" & ASCII.LF);
         return Failure;
      end if;

      --  Initialise the paging memory structures.
      Cxos.Serial.Put_String ("Initialising kernel page directory" & ASCII.LF);
      Init_Result := Initialise_Kernel_Page_Directory;
      if Init_Result /= Success then
         Cxos.Serial.Put_String (
           "Error initialising kernel page directory" & ASCII.LF);
         return Failure;
      end if;

      Cxos.Serial.Put_String ("Loading kernel page directory" & ASCII.LF);
      x86.Memory.Paging.Load_Page_Directory (Kernel_Page_Directory_Addr);
      Cxos.Serial.Put_String ("Kernel page directory loaded" & ASCII.LF);

      Cxos.Serial.Put_String ("Freeing boot page structures" & ASCII.LF);
      Init_Result := Clear_Boot_Page_Structures;
      if Init_Result /= Success then
         Cxos.Serial.Put_String (
           "Error freeing boot page structures" & ASCII.LF);
         return Failure;
      end if;

      --  Map VGA memory to a usable address.
      Cxos.Serial.Put_String ("Mapping VGA memory" & ASCII.LF);
      Init_Result := Map_Vga_Memory;
      if Init_Result /= Success then
         Cxos.Serial.Put_String ("Error mapping VGA memory" & ASCII.LF);
         return Failure;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Failure;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Initialise_Kernel_Page_Directory
   --
   --  Implementation Notes:
   --    - Assumes kernel can fit in one page table.
   --    - Assumes boot page table has been properly recursively mapped.
   ----------------------------------------------------------------------------
   function Initialise_Kernel_Page_Directory return Kernel_Process_Result is
      use System.Storage_Elements;
      use x86.Memory.Map;
      use x86.Memory.Paging;

      --  The result of the frame allocation operation.
      Allocate_Result : x86.Memory.Map.Process_Result;

      --  Boot page directory initialised during boot.
      --  The last page directory entry has been used to recursively map
      --  the directory, so this constant address is used here.
      Boot_Page_Directory : Page_Directory
      with Import,
        Convention => Ada,
        Address    => To_Address (16#FFFF_F000#);

      --  The page directory entry corresponding to the kernel's
      --  virtual memory offset.
      Boot_Kernel_Page_Table : Page_Table
      with Import,
        Convention => Ada,
        Address    => Convert_To_System_Address
          (Boot_Page_Directory (768).Table_Address);

      --  The allocated address of the first kernel page table.
      Kernel_Page_Table_Addr : System.Address;
   begin
      --  Allocate all required structures.
      Allocate_Frames :
         begin
            --  Allocate the Kernel page directory frame.
            --  This populates the Kernel page directory address with the
            --  address of the newly allocated frame.
            Allocate_Result := x86.Memory.Map.Allocate_Frame (
              Kernel_Page_Directory_Addr);
            if Allocate_Result /= Success then
               return Failure;
            end if;

            --  Allocate the initial kernel page table frame.
            Allocate_Result := x86.Memory.Map.Allocate_Frame (
              Kernel_Page_Table_Addr);
            if Allocate_Result /= Success then
               return Failure;
            end if;
         exception
            when Constraint_Error =>
               return Failure;
         end Allocate_Frames;

      --  Temporarily map the newly allocated page frame structures into the
      --  last entries in trhe boot page table.
      Initialise_Temporary_Mapping :
         begin
            --  Temporarily map the kernel page table to 0xC03FE000.
            Boot_Kernel_Page_Table (1022).Page_Address :=
              Convert_To_Page_Aligned_Address (Kernel_Page_Table_Addr);
            Boot_Kernel_Page_Table (1022).Present    := True;
            Boot_Kernel_Page_Table (1022).Read_Write := True;

            --  Temporarily map the kernel page dir to 0xC03FF000.
            Boot_Kernel_Page_Table (1023).Page_Address :=
              Convert_To_Page_Aligned_Address (Kernel_Page_Directory_Addr);
            Boot_Kernel_Page_Table (1023).Present    := True;
            Boot_Kernel_Page_Table (1023).Read_Write := True;

            --  Flush the TLB to make our changes effective.
            Flush_Tlb;
         exception
            when Constraint_Error =>
               return Failure;
         end Initialise_Temporary_Mapping;

      Init_Directory :
         declare
            --  The Kernel Page Directory.
            Kernel_Page_Directory : Page_Directory
            with Import,
              Convention => Ada,
              Address    => To_Address (16#C03F_F000#);

            --  The initial Kernel Page Table.
            Kernel_Page_Table : Page_Table
            with Import,
              Convention => Ada,
              Address    => To_Address (16#C03F_E000#);

            --  Process result of internal processes.
            Result : x86.Memory.Paging.Process_Result;
         begin
            --  Initialise the page directory.
            Init_Page_Structures :
               begin
                  Result := Initialise_Page_Table (Kernel_Page_Table);
                  if Result /= Success then
                     return Failure;
                  end if;

                  Result := Initialise_Page_Directory (Kernel_Page_Directory);
                  if Result /= Success then
                     return Failure;
                  end if;

                  --  Map the kernel page table.
                  Kernel_Page_Directory (768).Present := True;
                  Kernel_Page_Directory (768).Table_Address :=
                    Convert_To_Page_Aligned_Address (Kernel_Page_Table_Addr);

                  --  Map the page index to the second last entry.
                  Kernel_Page_Directory (1023).Present := True;
                  Kernel_Page_Directory (1023).Table_Address :=
                    Convert_To_Page_Aligned_Address (
                    Kernel_Page_Directory_Addr);
               exception
                  when Constraint_Error =>
                     return Failure;
               end Init_Page_Structures;

            --  Map the kernel address space.
            Map_Kernel_Address_Space :
               declare
                  --  The current Address being mapped.
                  Current_Addr       : Integer_Address;
                  --  The length of the kernel code segment in bytes.
                  Kernel_Length      : Integer;
                  --  The number of frames in the kernel.
                  Kernel_Frame_Count : Integer;
                  --  The page table index to begin mapping from.
                  --  This corresponds to the frame the kernel memory begins
                  --  at relative to the virtual memory starting offset.
                  --    e.g (0xC0000000 - 0xC0100000) / 0x1000 = 256;
                  Start_Frame        : Integer;

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
                  --  The address of the kernel in virtual memory.
                  Kernel_Vma_Start : constant Unsigned_32
                  with Import,
                    Convention    => Assembler,
                    External_name => "KERNEL_VMA_START";
               begin
                  Kernel_Length := Integer (
                    To_Integer (Kernel_End'Address) -
                    To_Integer (Kernel_Start'Address));

                  --  The kernel's physical start is the virtual memory
                  --  logical start subtracted from the kernel memory start.
                  Current_Addr :=
                    To_Integer (Kernel_Start'Address) -
                    To_Integer (Kernel_Vma_Start'Address);

                  Kernel_Frame_Count := 1 + (Kernel_Length / 16#1000#);
                  Start_Frame        := Integer (Current_Addr / 16#1000#);

                  for I in 0 .. Kernel_Frame_Count loop
                     Kernel_Page_Table (Start_Frame + I).Present := True;
                     Kernel_Page_Table (Start_Frame + I).Page_Address :=
                       Convert_To_Page_Aligned_Address (
                       To_Address (Current_Addr));

                     --  Increment the counter by one page frame in size.
                     Current_Addr := Current_Addr + 16#1000#;
                  end loop;
               exception
                  when Constraint_Error =>
                     return Failure;
               end Map_Kernel_Address_Space;
         end Init_Directory;

         return Success;
   end Initialise_Kernel_Page_Directory;

   ----------------------------------------------------------------------------
   --  Map_Vga_Memory
   ----------------------------------------------------------------------------
   function Map_Vga_Memory return Kernel_Process_Result is
      use System.Storage_Elements;
      use x86.Memory.Paging;

      --  The result of the mapping process.
      Result : x86.Memory.Paging.Process_Result;
   begin
      Result := x86.Memory.Paging.Map_Page_Frame (To_Address (16#B8000#),
        To_Address (16#C03F_E000#));
      if Result /= Success then
         return Failure;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Failure;
   end Map_Vga_Memory;

   ----------------------------------------------------------------------------
   --  Mark_Kernel_Memory
   --
   --  Implementation Notes:
   --    - Marks the kernel's physical memory as being used.
   ----------------------------------------------------------------------------
   function Mark_Kernel_Memory return Kernel_Process_Result is
      use System.Storage_Elements;
      use x86.Memory.Map;

      --  The length of the kernel code segment in bytes.
      Kernel_Length    : Unsigned_32 := 0;

      --  The result of the frame status set process.
      Result : x86.Memory.Map.Process_Result := Success;

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
      --  The address of the kernel in virtual memory.
      Kernel_Vma_Start : constant Unsigned_32
      with Import,
        Convention    => Assembler,
        External_name => "KERNEL_VMA_START";

      --  The physical start of Kernel memory.
      Kernel_Physical_Start : System.Address := To_Address (0);
   begin
      Kernel_Length   := Unsigned_32 (
        To_Integer (Kernel_End'Address) -
        To_Integer (Kernel_Start'Address));

      --  The kernel's physical start is the virtual memory logical start
      --  subtracted from the kernel memory start.
      Kernel_Physical_Start := To_Address (
        To_Integer (Kernel_Start'Address) -
        To_Integer (Kernel_Vma_Start'Address));

      Result := x86.Memory.Map.Mark_Memory_Range (
        Kernel_Physical_Start, Kernel_Length, Allocated);
      if Result /= Success then
         Cxos.Serial.Put_String (
           "Error marking kernel code segment" & ASCII.LF);

         return Failure;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Failure;
   end Mark_Kernel_Memory;

   ----------------------------------------------------------------------------
   --  Mark_Low_Memory
   ----------------------------------------------------------------------------
   function Mark_Low_Memory return Kernel_Process_Result is
      use System.Storage_Elements;
      use x86.Memory.Map;

      --  The result of the process.
      Result : x86.Memory.Map.Process_Result := Success;
   begin
      Result := x86.Memory.Map.Mark_Memory_Range (To_Address (0),
        16#100000#, Allocated);

      if Result /= Success then
         Cxos.Serial.Put_String ("Error setting memory range" & ASCII.LF);

         return Failure;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Failure;
   end Mark_Low_Memory;

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Memory_Map
   ----------------------------------------------------------------------------
   procedure Parse_Multiboot_Memory_Map (
     Memory_Map_Addr   : System.Address;
     Memory_Map_Length : Unsigned_32
   ) is
      use System.Storage_Elements;
      use x86.Memory.Map;

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
               Cxos.Serial.Put_String ("Parsing Mmap region" & ASCII.LF);
               Cxos.Serial.Put_String ("  Type:  ");

               case Curr_Region.all.Memory_Type is
                  when 1 =>
                     Cxos.Serial.Put_String ("Free RAM" & ASCII.LF);
                  when 3 =>
                     Cxos.Serial.Put_String ("ACPI" & ASCII.LF);
                  when 4 =>
                     Cxos.Serial.Put_String (
                       "Reserved for hibernation" & ASCII.LF);
                  when 5 =>
                     Cxos.Serial.Put_String ("Defective" & ASCII.LF);
                  when others =>
                     Cxos.Serial.Put_String ("Reserved" & ASCII.LF);
               end case;

               Cxos.Serial.Put_String ("  Base:   " &
                 Unsigned_32 (Curr_Region.all.Base and 16#FFFF_FFFF#)'Image &
                 ASCII.LF);
               Cxos.Serial.Put_String ("  Length: " &
                 Unsigned_32 (Curr_Region.all.Length and 16#FFFF_FFFF#)'Image &
                 ASCII.LF);
            exception
               when Constraint_Error =>
                  return;
            end Print_Memory_Region_Info;

         --  Mark free memory in the kernel memory map.
         Mark_Free_Memory :
            declare

               --  The result of the frame status set process.
               Result : x86.Memory.Map.Process_Result := Success;
            begin
               --  If the memory region is marked as free, set the status
               --  accordingly in the memory map.
               case Curr_Region.all.Memory_Type is
                  when 1 =>
                     Result := x86.Memory.Map.Mark_Memory_Range (
                       To_Address (Integer_Address (Curr_Region.all.Base)),
                       Unsigned_32 (Curr_Region.all.Length), Unallocated);
                     if Result /= Success then
                        Cxos.Serial.Put_String (
                          "Error setting frame status" & ASCII.LF);
                        return;
                     end if;
                  when others =>
                     null;
               end case;
            exception
               when Constraint_Error =>
                  Cxos.Serial.Put_String (
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
end Cxos.Memory;
