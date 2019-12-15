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
--  CXOS
--
--  Purpose:
--    This package contains the main Kernel code.
-------------------------------------------------------------------------------
package Cxos is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Initialise_Kernel
   --
   --  Purpose:
   --    Initialises the kernel.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise_Kernel
   with Export,
     Convention    => Assembler,
     External_Name => "__kernel_init";

   ----------------------------------------------------------------------------
   --  Main
   --
   --  Purpose:
   --    The main kernel loop.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Main
   with No_Return;

   ----------------------------------------------------------------------------
   --  Print_Splash
   --
   --  Purpose:
   --    Prints the CXOS test splash screen.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Print_Splash;
end Cxos;
