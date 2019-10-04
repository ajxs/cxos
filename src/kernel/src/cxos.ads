-------------------------------------------------------------------------------
--  CXOS
--
--  Purpose:
--    This package contains the main Kernel code.
-------------------------------------------------------------------------------
package Cxos is
   pragma Preelaborate (Cxos);

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
end Cxos;
