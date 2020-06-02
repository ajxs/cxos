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
--  CXOS.BOOT
--
--  Purpose:
--    This package contains the kernel boot code.
-------------------------------------------------------------------------------
package Cxos.Boot is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  Program status type.
   --  Used to track the result of individual functions.
   ----------------------------------------------------------------------------
   type Program_Status is (
     Success,
     Unhandled_Exception,
     Unset
   );

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

private
   ----------------------------------------------------------------------------
   --  Mark_Kernel_Memory
   --
   --  Purpose:
   --    This procedure marks the memory used by the kernel as being allocated
   --    and non-free in the memory map.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Mark_Kernel_Memory (Status : out Program_Status);

   ----------------------------------------------------------------------------
   --  Protected_Mode_Init
   --
   --  Purpose:
   --    Performs the final jump to protected mode.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Protected_Mode_Init
   with Import,
     Convention    => Assembler,
     External_Name => "__protected_mode_init";

end Cxos.Boot;
