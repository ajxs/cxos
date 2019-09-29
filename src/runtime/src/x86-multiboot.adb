with x86.Vga;

package body x86.Multiboot is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise (
     Magic_Number      : Multiboot_Magic_Number;
     Boot_Info_Address : System.Address
   ) is
      use type x86.Vga.Color;

      --  Create multiboot info structure overlaid at boot info address.
      Boot_Info : Multiboot_Info
      with Address => Boot_Info_Address,
        Import,
        Convention => Ada,
        Volatile;

      Terminal_Background : constant x86.Vga.Color := x86.Vga.Black;
   begin
      x86.Vga.Clear (Terminal_Background);

      --  Check whether we were booted by a Multiboot compatible bootloader.
      if Magic_Number = VALID_MAGIC_NUMBER then
         x86.Vga.Put_String (0, 0, x86.Vga.Green, Terminal_Background,
           "Detected valid Multiboot magic number");
      else
         x86.Vga.Put_String (0, 0, x86.Vga.Red, Terminal_Background,
           "Unable to detect valid Multiboot magic number");
      end if;
   end Initialise;
end x86.Multiboot;
