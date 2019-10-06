with System.Machine_Code;

package body Cxos is
   ----------------------------------------------------------------------------
   --  Main
   ----------------------------------------------------------------------------
   procedure Main is
   begin
      --  Loop forever.
      loop
         System.Machine_Code.Asm ("hlt", Volatile => True);
      end loop;
   end Main;
end Cxos;
