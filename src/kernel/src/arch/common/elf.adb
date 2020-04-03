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

package body Elf is
   ----------------------------------------------------------------------------
   --  Validate_Elf_Header_Magic_Number
   ----------------------------------------------------------------------------
   function Validate_Elf_Header_Magic_Number (
     Magic_Number : Elf_File_Magic_Number
   ) return Boolean is
      --  The expected value for the ELF Magic Number.
      Elf_Header_Magic_Number : constant Elf_File_Magic_Number
        := Character'Val (16#7F#) & "ELF";
   begin
      return Magic_Number = Elf_Header_Magic_Number;
   end Validate_Elf_Header_Magic_Number;
end Elf;
