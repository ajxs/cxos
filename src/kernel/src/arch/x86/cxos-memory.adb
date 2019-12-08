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

with Cxos.Memory.Paging;
with Cxos.Serial;
with Interfaces;
with System.Storage_Elements;
with Cxos.Memory.Map;
with x86.Memory.Paging;

package body Cxos.Memory is
   use Interfaces;

   ----------------------------------------------------------------------------
   --  Clear_Boot_Page_Structures
   ----------------------------------------------------------------------------
   function Clear_Boot_Page_Structures return Process_Result is
      use Cxos.Memory.Map;

      --  The result of the frame status set process.
      Result : Process_Result;

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
      Result := Cxos.Memory.Map.Mark_Memory_Range (
        Boot_Page_Directory'Address, 16#1000#, Unallocated);
      if Result /= Success then
         Cxos.Serial.Put_String (
           "Error freeing boot page directory" & ASCII.LF);
         return Unhandled_Exception;
      end if;

      Result := Cxos.Memory.Map.Mark_Memory_Range (
        Boot_Page_Table'Address, 16#1000#, Unallocated);
      if Result /= Success then
         Cxos.Serial.Put_String (
           "Error freeing boot page table" & ASCII.LF);
         return Unhandled_Exception;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Clear_Boot_Page_Structures;

   ----------------------------------------------------------------------------
   --  Create_Page_Directory
   ----------------------------------------------------------------------------
   function Create_Page_Directory (
     Page_Directory_Addr : out System.Address
   ) return Process_Result is
      use System.Storage_Elements;
      use x86.Memory.Paging;
   begin
      --  Allocate all required structures.
      Allocate_Frames :
         declare
            --  The result of the frame allocation operation.
            Allocate_Result : Process_Result;
         begin
            --  Allocate the Kernel page directory frame.
            --  This populates the Kernel page directory address with the
            --  address of the newly allocated frame.
            Allocate_Result := Cxos.Memory.Map.
              Allocate_Frame (Page_Directory_Addr);
            if Allocate_Result /= Success then
               return Unhandled_Exception;
            end if;
         exception
            when Constraint_Error =>
               return Unhandled_Exception;
         end Allocate_Frames;

      Initialise_Structures :
         declare
            --  The currently loaded page directory.
            --  The last page directory entry has been used to recursively map
            --  the directory, so this constant address is used here.
            Current_Page_Directory : Page_Directory
            with Import,
              Convention => Ada,
              Address    => To_Address (
                Cxos.Memory.Paging.PAGE_DIR_RECURSIVE_MAP_ADDR);

            --  The first kernel page directory in the currently loaded
            --  virtual address space.
            Kernel_Page_Table      : Page_Table
            with Import,
              Convention => Ada,
              Address    => Convert_To_System_Address
                (Current_Page_Directory (768).Table_Address);

            --  The newly allocated Page Directory.
            --  This will be mapped to the second last entry in the currently
            --  loaded kernel page table, mapping to 16#CO3F_F000#.
            New_Page_Directory : Page_Directory
            with Import,
              Convention => Ada,
              Address    => To_Address (16#C03F_F000#);

            --  The result of initialising the newly allocated page directory.
            Init_Result : x86.Memory.Paging.Process_Result;
         begin
            --  Temporarily map the kernel page dir to 0xC03FF000.
            Kernel_Page_Table (1023).Page_Address :=
              Convert_To_Page_Aligned_Address (Page_Directory_Addr);
            Kernel_Page_Table (1023).Present    := True;
            Kernel_Page_Table (1023).Read_Write := True;

            --  Flush the TLB to load the new mapping.
            Flush_Tlb;

            Init_Result := Initialise_Page_Directory (New_Page_Directory);
            if Init_Result /= Success then
               return Unhandled_Exception;
            end if;

            --  Recursively map the page directory last entry.
            New_Page_Directory (1023).Present := True;
            New_Page_Directory (1023).Table_Address :=
              Convert_To_Page_Aligned_Address (Page_Directory_Addr);

         end Initialise_Structures;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Create_Page_Directory;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   function Initialise return Process_Result is
   begin
      --  Mark used memory.
      Map_System_Memory :
         declare
            --  The result of the internal processes.
            Init_Result : Process_Result;
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
               return Unhandled_Exception;
         end Map_System_Memory;

      Init_Paging_Structures :
         declare
            --  The result of the internal processes.
            Init_Result : Process_Result;
         begin
            --  Initialise the paging memory structures.
            Cxos.Serial.Put_String ("Initialising kernel page directory"
              & ASCII.LF);
            Init_Result := Initialise_Kernel_Page_Directory;
            if Init_Result /= Success then
               Cxos.Serial.Put_String (
                 "Error initialising kernel page directory" & ASCII.LF);
               return Unhandled_Exception;
            end if;

            Cxos.Serial.Put_String ("Loading kernel page directory"
              & ASCII.LF);
            Cxos.Memory.Paging.Load_Page_Directory (
              Kernel_Page_Directory_Addr);
            Cxos.Serial.Put_String ("Kernel page directory loaded" & ASCII.LF);

            Cxos.Serial.Put_String ("Freeing boot page structures" & ASCII.LF);
            Init_Result := Clear_Boot_Page_Structures;
            if Init_Result /= Success then
               Cxos.Serial.Put_String (
                 "Error freeing boot page structures" & ASCII.LF);
               return Unhandled_Exception;
            end if;

            --  Map VGA memory to a usable address.
            Cxos.Serial.Put_String ("Mapping VGA memory" & ASCII.LF);
            Init_Result := Map_Vga_Memory;
            if Init_Result /= Success then
               Cxos.Serial.Put_String ("Error mapping VGA memory" & ASCII.LF);
               return Unhandled_Exception;
            end if;
         end Init_Paging_Structures;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Initialise_Kernel_Page_Directory
   --
   --  Implementation Notes:
   --    - Assumes kernel can fit in one page table.
   --    - Assumes boot page table has been properly recursively mapped.
   ----------------------------------------------------------------------------
   function Initialise_Kernel_Page_Directory return Process_Result is
      use System.Storage_Elements;
      use x86.Memory.Paging;

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
         declare
            --  The result of the frame allocation operation.
            Allocate_Result : Process_Result;
         begin
            --  Allocate the Kernel page directory frame.
            --  This populates the Kernel page directory address with the
            --  address of the newly allocated frame.
            Allocate_Result := Cxos.Memory.Map.Allocate_Frame (
              Kernel_Page_Directory_Addr);
            if Allocate_Result /= Success then
               return Unhandled_Exception;
            end if;

            --  Allocate the initial kernel page table frame.
            Allocate_Result := Cxos.Memory.Map.Allocate_Frame (
              Kernel_Page_Table_Addr);
            if Allocate_Result /= Success then
               return Unhandled_Exception;
            end if;
         exception
            when Constraint_Error =>
               return Unhandled_Exception;
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
               return Unhandled_Exception;
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
                     return Unhandled_Exception;
                  end if;

                  Result := Initialise_Page_Directory (Kernel_Page_Directory);
                  if Result /= Success then
                     return Unhandled_Exception;
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
                     return Unhandled_Exception;
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
                     return Unhandled_Exception;
               end Map_Kernel_Address_Space;
         end Init_Directory;

         return Success;
   end Initialise_Kernel_Page_Directory;

   ----------------------------------------------------------------------------
   --  Map_Vga_Memory
   ----------------------------------------------------------------------------
   function Map_Vga_Memory return Process_Result is
      use System.Storage_Elements;

      --  The result of the mapping process.
      Result : Process_Result;
   begin
      Result := Cxos.Memory.Paging.Map_Page_Frame (To_Address (16#B8000#),
        To_Address (16#C03F_E000#));
      if Result /= Success then
         return Unhandled_Exception;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Map_Vga_Memory;

   ----------------------------------------------------------------------------
   --  Mark_Kernel_Memory
   --
   --  Implementation Notes:
   --    - Marks the kernel's physical memory as being used.
   ----------------------------------------------------------------------------
   function Mark_Kernel_Memory return Process_Result is
      use System.Storage_Elements;
      use Cxos.Memory.Map;

      --  The length of the kernel code segment in bytes.
      Kernel_Length    : Unsigned_32 := 0;

      --  The result of the frame status set process.
      Result : Process_Result := Success;

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

      Result := Cxos.Memory.Map.Mark_Memory_Range (
        Kernel_Physical_Start, Kernel_Length, Allocated);
      if Result /= Success then
         Cxos.Serial.Put_String (
           "Error marking kernel code segment" & ASCII.LF);

         return Unhandled_Exception;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Mark_Kernel_Memory;

   ----------------------------------------------------------------------------
   --  Mark_Low_Memory
   ----------------------------------------------------------------------------
   function Mark_Low_Memory return Process_Result is
      use System.Storage_Elements;
      use Cxos.Memory.Map;

      --  The result of the process.
      Result : Process_Result := Success;
   begin
      Result := Cxos.Memory.Map.Mark_Memory_Range (To_Address (0),
        16#100000#, Allocated);

      if Result /= Success then
         return Result;
      end if;

      return Success;
   exception
      when Constraint_Error =>
         return Unhandled_Exception;
   end Mark_Low_Memory;

end Cxos.Memory;
