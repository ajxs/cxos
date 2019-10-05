with System.Machine_Code;

package body System.x86.Port_IO is
   ----------------------------------------------------------------------------
   --  Inb
   ----------------------------------------------------------------------------
   function Inb (
     Port : System.Address
   ) return Interfaces.Unsigned_8 is
      Data : Interfaces.Unsigned_8;
   begin
      System.Machine_Code.Asm (
        Template => "inb %w1, %0",
        Inputs => (
          System.Address'Asm_Input ("Nd", Port)
        ),
        Outputs => (
          Interfaces.Unsigned_8'Asm_Output ("=a", Data)
        ),
        Volatile => True);

      return Data;
   end Inb;

   ----------------------------------------------------------------------------
   --  Outb
   ----------------------------------------------------------------------------
   procedure Outb (
     Port : System.Address;
     Data : Interfaces.Unsigned_8
   ) is
   begin
      System.Machine_Code.Asm (
        Template => "outb %0, %w1",
        Inputs => (
          Interfaces.Unsigned_8'Asm_Input ("a", Data),
          System.Address'Asm_Input ("Nd", Port)
        ),
        Volatile => True);
   end Outb;
end System.x86.Port_IO;
