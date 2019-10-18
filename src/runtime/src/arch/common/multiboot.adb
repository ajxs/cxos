with System.Storage_Elements;

package body Multiboot is
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Parse_Memory_Map
   ----------------------------------------------------------------------------
   procedure Parse_Memory_Map (
     Memory_Map_Addr   : Unsigned_32;
     Memory_Map_Length : Unsigned_32
   ) is
      Next_Structure_Address : System.Address := To_Address (0);

      procedure Read_Memory_Map_Structure (
        Struct_Addr : System.Address
      );

      procedure Read_Memory_Map_Structure (
        Struct_Addr : System.Address
      ) is
      begin
         null;
      end Read_Memory_Map_Structure;
   begin
      pragma Unreferenced (Next_Structure_Address);
      pragma Unreferenced (Memory_Map_Addr);
      pragma Unreferenced (Memory_Map_Length);
      pragma Unreferenced (Read_Memory_Map_Structure);
      null;
   end Parse_Memory_Map;

end Multiboot;
