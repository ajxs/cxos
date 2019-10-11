with x86.Serial;

package body Cxos.Serial is
   ----------------------------------------------------------------------------
   --  Put_String
   ----------------------------------------------------------------------------
   procedure Put_String (
     Data : String
   ) is
   begin
      x86.Serial.Put_String(x86.Serial.COM1, Data);
   end;
end Cxos.Serial;
