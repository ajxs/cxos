with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Multiboot is
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Parse_Memory_Map
   ----------------------------------------------------------------------------
   procedure Parse_Memory_Map (
     Memory_Map_Addr   : System.Address;
     Memory_Map_Length : Unsigned_32
   ) is
      package Mmap_Region_Ptr is new
        System.Address_To_Access_Conversions (Multiboot_Mmap_Region);

      Bytes_Read  : Unsigned_32    := 0;
      Curr_Addr   : System.Address := Memory_Map_Addr;
      Curr_Region : Mmap_Region_Ptr.Object_Pointer :=
        Mmap_Region_Ptr.To_Pointer (Curr_Addr);
   begin
      while Bytes_Read < Memory_Map_Length loop
         --  Reset the current region pointer.
         Curr_Region := Mmap_Region_Ptr.To_Pointer (Curr_Addr);

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
   end Parse_Memory_Map;

end Multiboot;
