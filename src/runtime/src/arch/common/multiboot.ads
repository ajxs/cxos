with Interfaces;
with System;

-------------------------------------------------------------------------------
--  MULTIBOOT
--
--  Purpose:
--    This package contains code for working with multiboot functionality and
--    data structures.
-------------------------------------------------------------------------------
package Multiboot is
   pragma Preelaborate (Multiboot);

   use Interfaces;

   type Syms_Array is array (0 .. 2) of Unsigned_32;

   type Color_Info_Type is array (0 .. 5) of Unsigned_8;

   ----------------------------------------------------------------------------
   --  Multiboot information flags struct.
   --  Map of the flags passed in the boot information struct.
   ----------------------------------------------------------------------------
   type Multiboot_Info_Flags is
      record
         Memory_Fields_Valid         : Boolean;
         Boot_Device_Field_Valid     : Boolean;
         Cmdline_Field_Valid         : Boolean;
         Mods_Fields_Valid           : Boolean;
         Aout_Fields_Valid           : Boolean;
         Elf_Fields_Valid            : Boolean;
         Memory_Map_Fields_Valid     : Boolean;
         Drives_Fields_Valid         : Boolean;
         Config_Table_Valid          : Boolean;
         Bootloader_name_Valid       : Boolean;
         Apm_Table_Valid             : Boolean;
         Vbe_Table_Available         : Boolean;
         Framebuffer_Table_Available : Boolean;
      end record
   with Size => 32;
   for Multiboot_Info_Flags use
      record
         Memory_Fields_Valid          at 0 range 0  .. 0;
         Boot_Device_Field_Valid      at 0 range 1  .. 1;
         Cmdline_Field_Valid          at 0 range 2  .. 2;
         Mods_Fields_Valid            at 0 range 3  .. 3;
         Aout_Fields_Valid            at 0 range 4  .. 4;
         Elf_Fields_Valid             at 0 range 5  .. 5;
         Memory_Map_Fields_Valid      at 0 range 6  .. 6;
         Drives_Fields_Valid          at 0 range 7  .. 7;
         Config_Table_Valid           at 0 range 8  .. 8;
         Bootloader_name_Valid        at 0 range 9  .. 9;
         Apm_Table_Valid              at 0 range 10 .. 10;
         Vbe_Table_Available          at 0 range 11 .. 11;
         Framebuffer_Table_Available  at 0 range 12 .. 12;
      end record;

   ----------------------------------------------------------------------------
   --  Multiboot information struct.
   --  As defined in the Multiboot spec in the GNU GRUB manual.
   ----------------------------------------------------------------------------
   type Multiboot_Info is
      record
         Flags              : Multiboot_Info_Flags;
         Mem_Upper          : System.Address;
         Mem_Lower          : System.Address;
         Boot_Device        : Unsigned_32;
         Cmdline            : Unsigned_32;
         Mods_Count         : Unsigned_32;
         Mods_Addr          : Unsigned_32;
         Syms               : Syms_Array;
         Mmap_Length        : Unsigned_32;
         Mmap_Addr          : Unsigned_32;
         Drives_Length      : Unsigned_32;
         Drives_Addr        : Unsigned_32;
         Config_Table       : Unsigned_32;
         Boot_Loader_Name   : Unsigned_32;
         APM_Table          : Unsigned_32;
         VBE_Control_Info   : Unsigned_32;
         VBE_Mode_Info      : Unsigned_32;
         VBE_Mode           : Unsigned_16;
         VBE_Interface_Seg  : Unsigned_16;
         VBE_Interface_Off  : Unsigned_16;
         VBE_Interface_Len  : Unsigned_16;
         Framebuffer_Addr   : Unsigned_64;
         Framebuffer_Pitch  : Unsigned_32;
         Framebuffer_Width  : Unsigned_32;
         Framebuffer_Height : Unsigned_32;
         Framebuffer_Bpp    : Boolean;
         Framebuffer_Type   : Boolean;
         Color_Info         : Color_Info_Type;
      end record
   with Size => 928;
   for Multiboot_Info use
      record
         Flags              at 0   range 0 .. 31;
         Mem_Upper          at 4   range 0 .. 31;
         Mem_Lower          at 8   range 0 .. 31;
         Boot_Device        at 12  range 0 .. 31;
         Cmdline            at 16  range 0 .. 31;
         Mods_Count         at 20  range 0 .. 31;
         Mods_Addr          at 24  range 0 .. 31;
         Syms               at 28  range 0 .. 95;
         Mmap_Length        at 44  range 0 .. 31;
         Mmap_Addr          at 48  range 0 .. 31;
         Drives_Length      at 52  range 0 .. 31;
         Drives_Addr        at 56  range 0 .. 31;
         Config_Table       at 60  range 0 .. 31;
         Boot_Loader_Name   at 64  range 0 .. 31;
         APM_Table          at 68  range 0 .. 31;
         VBE_Control_Info   at 72  range 0 .. 31;
         VBE_Mode_Info      at 76  range 0 .. 31;
         VBE_Mode           at 80  range 0 .. 15;
         VBE_Interface_Seg  at 82  range 0 .. 15;
         VBE_Interface_Off  at 84  range 0 .. 15;
         VBE_Interface_Len  at 86  range 0 .. 15;
         Framebuffer_Addr   at 88  range 0 .. 63;
         Framebuffer_Pitch  at 96  range 0 .. 31;
         Framebuffer_Width  at 100 range 0 .. 31;
         Framebuffer_Height at 104 range 0 .. 31;
         Framebuffer_Bpp    at 108 range 0 .. 7;
         Framebuffer_Type   at 109 range 0 .. 7;
         Color_Info         at 110 range 0 .. 47;
      end record;

   ----------------------------------------------------------------------------
   --  Multiboot magic number.
   --  As defined iboot spec in the GNU GRUB manual.
   --  Used for validation that the bootloader has correctly loaded the kernel.
   ----------------------------------------------------------------------------
   subtype Multiboot_Magic_Number is Unsigned_32;

   ----------------------------------------------------------------------------
   --  The expected, valid Multiboot magic number value.
   ----------------------------------------------------------------------------
   VALID_MAGIC_NUMBER : constant Multiboot_Magic_Number := 16#2BADB002#;

end Multiboot;
