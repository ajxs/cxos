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

package body x86.Paging is
   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Implementation Notes:
   --   - Initialises every page table as being non present and non writeable.
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      --  Initialise the page table structure.
      --  Initially all tables are marked as non-present.
      Initialise_Page_Tables :
         begin
            --  Initialise each table in the page table structure.
            for Table of Page_Tables loop
               --  Initialise each entry in this page table.
               for PT_Entry of Table loop
                  PT_Entry.Present      := False;
                  PT_Entry.Read_Write   := False;
                  PT_Entry.U_S          := False;
                  PT_Entry.PWT          := False;
                  PT_Entry.PCD          := False;
                  PT_Entry.A            := False;
                  PT_Entry.Page_Address := 0;
               end loop;
            end loop;
         end Initialise_Page_Tables;
   end Initialise;
end x86.Paging;
