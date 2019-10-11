with Cxos;

-------------------------------------------------------------------------------
--  Kernel_Entry
--
--  Purpose:
--    The Kernel_Entry procedure is the main entry point for the kernel.
--    This procedure is called from the boot code contained within the
--    kernel's Ada runtime, and serves as the transition between the boot
--    code and the kernel functionality.
--
-------------------------------------------------------------------------------
procedure Kernel_Entry is
begin
   --  Call the main kernel function.
   Cxos.Main;
end Kernel_Entry;
