with System.x86.Vga;

package body System.x86.Multiboot is
   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise (
     Magic_Number      : Multiboot_Magic_Number;
     Boot_Info_Address : System.Address
   ) is
      use type System.x86.Vga.Color;

      --  Create multiboot info structure overlaid at boot info address.
      Boot_Info : Multiboot_Info
      with Address => Boot_Info_Address,
        Import,
        Convention => Ada,
        Volatile;

      Terminal_Background : constant System.x86.Vga.Color := System.x86.Vga.Black;
   begin
      System.x86.Vga.Clear (Terminal_Background);

      --  Check whether we were booted by a Multiboot compatible bootloader.
      if Magic_Number = VALID_MAGIC_NUMBER then
         System.x86.Vga.Put_String (0, 0, System.x86.Vga.Green, Terminal_Background,
           "Detected valid Multiboot magic number");
      else
         System.x86.Vga.Put_String (0, 0, System.x86.Vga.Red, Terminal_Background,
           "Unable to detect valid Multiboot magic number");
      end if;
   end Initialise;
end System.x86.Multiboot;
