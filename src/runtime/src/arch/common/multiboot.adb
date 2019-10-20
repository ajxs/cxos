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
      package Memory_Map_Section_Ptr is new
        System.Address_To_Access_Conversions (Multiboot_Memory_Map_Section);

      Bytes_Read : Unsigned_32 := 0;
      Current_Addr : System.Address := Memory_Map_Addr;
      Curr_Section_Ptr : Memory_Map_Section_Ptr.Object_Pointer :=
        Memory_Map_Section_Ptr.To_Pointer (Current_Addr);
   begin
      while Bytes_Read < Memory_Map_Length loop
         Curr_Section_Ptr :=
           Memory_Map_Section_Ptr.To_Pointer (Current_Addr);

         Increment_Pointer :
            begin
               Current_Addr := To_Address (To_Integer (Current_Addr) +
                 Integer_Address (4 + Curr_Section_Ptr.all.Size));

               Bytes_Read := Bytes_Read + 4 + Curr_Section_Ptr.all.Size;
            exception
               when Constraint_Error =>
                  return;
            end Increment_Pointer;
      end loop;
   end Parse_Memory_Map;

end Multiboot;
