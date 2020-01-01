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

with Interfaces;
with System;

-------------------------------------------------------------------------------
--  CXOS.MULTIBOOT_INIT
--
--  Purpose:
--    This package contains code and defintions for working with the
--    multiboot information structures provided at system init by the
--    bootloader.
-------------------------------------------------------------------------------
package Cxos.Multiboot_Init is
   pragma Preelaborate;

   use Interfaces;

   ----------------------------------------------------------------------------
   --  Process Result type.
   --  Used for storing and returning the result of an internal memory related
   --  procedure.
   ----------------------------------------------------------------------------
   type Process_Result is (
     Success,
     Unhandled_Exception
   );

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Info
   --
   --  Purpose:
   --    Parses the multiboot info structures.
   ----------------------------------------------------------------------------
   function Parse_Multiboot_Info return Process_Result;

private
   ----------------------------------------------------------------------------
   --  Multiboot section information type.
   --  Contains information necessary to load a particular multiboot section
   --  that has been copied to an alternate location during boot.
   ----------------------------------------------------------------------------
   type Multiboot_Section_Info is
      record
         Section_Addr   : System.Address;
         Section_Length : Unsigned_32;
      end record
   with Size => 64;
   for Multiboot_Section_Info use
      record
         Section_Addr   at 0 range 0 .. 31;
         Section_Length at 4 range 0 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Memory_Map
   --
   --  Purpose:
   --    Parses the multiboot memory map structures, mapping the specified
   --    memory regions listed in the multiboot structure.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Parse_Multiboot_Memory_Map (
     Memory_Map_Addr   : System.Address;
     Memory_Map_Length : Unsigned_32
   ) return Process_Result;

   ----------------------------------------------------------------------------
   --  Parse_Multiboot_Drive_Map
   --
   --  Purpose:
   --    This function parses the Multiboot information structure's drive map.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Parse_Multiboot_Drive_Map (
     Drive_Map_Addr   : System.Address;
     Drive_Map_Length : Unsigned_32
   ) return Process_Result;

end Cxos.Multiboot_Init;
