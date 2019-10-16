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

-------------------------------------------------------------------------------
--  SYSTEM.X86.EXCEPTIONS.TESTING
--
--  Purpose:
--    This package contains code for testing handling of x86 processor
--    exceptions.
-------------------------------------------------------------------------------
package x86.Exceptions.Testing is
   pragma Preelaborate (x86.Exceptions.Testing);

   ----------------------------------------------------------------------------
   --  Test_Divide_By_Zero
   --
   --  Purpose:
   --    This procedure triggers a processor divide-by-zero exception.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Test_Divide_By_Zero
   with Import,
     Convention    => Assembler,
     External_Name => "__test_div0";

end x86.Exceptions.Testing;
