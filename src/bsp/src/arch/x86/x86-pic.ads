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

with System;

-------------------------------------------------------------------------------
--  X86.PIC
--
--  Purpose:
--    This package contains code for working with the x86 8259A programmable
--    interrupt controller.
-------------------------------------------------------------------------------
package x86.PIC is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  PIC Controller type.
   --  Used in selecting which PIC to perform an operation on.
   ----------------------------------------------------------------------------
   type PIC_Controller_T is (
     PIC1,
     PIC2
   );

   ----------------------------------------------------------------------------
   --  Get_Controller_Base_Address
   --
   --  Purpose:
   --    This function gets the base address for a particular PIC controller.
   ----------------------------------------------------------------------------
   function Get_Controller_Base_Address (
      Controller : PIC_Controller_T
   ) return System.Address;

end x86.PIC;
