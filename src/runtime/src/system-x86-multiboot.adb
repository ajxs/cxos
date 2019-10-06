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

      Terminal_Foreground : constant System.x86.Vga.Color :=
        System.x86.Vga.Light_Green;
      Terminal_Background : constant System.x86.Vga.Color :=
        System.x86.Vga.Black;
   begin
      System.x86.Vga.Clear (Terminal_Background);

      --  Check whether we were booted by a Multiboot compatible bootloader.
      if Magic_Number = VALID_MAGIC_NUMBER then
         System.x86.Vga.Put_String (0, 0, System.x86.Vga.Green,
           Terminal_Background, "Detected valid Multiboot magic number");
      else
         System.x86.Vga.Put_String (0, 0, System.x86.Vga.Red,
           Terminal_Background,
           "Unable to detect valid Multiboot magic number");
      end if;

      Print_Vga_Splash :
         begin
            System.x86.Vga.Clear (Terminal_Background);

            --  Print ASCII art test screen.
            System.x86.Vga.Put_String (1, 1, Terminal_Foreground,
              Terminal_Background, "  /$$$$$$  /$$   /$$  /$$$$$$   /$$$$$$ ");
            System.x86.Vga.Put_String (1, 2, Terminal_Foreground,
              Terminal_Background, " /$$__  $$| $$  / $$ /$$__  $$ /$$__  $$");
            System.x86.Vga.Put_String (1, 3, Terminal_Foreground,
              Terminal_Background, "| $$  \__/|  $$/ $$/| $$  \ $$| $$  \__/");
            System.x86.Vga.Put_String (1, 4, Terminal_Foreground,
              Terminal_Background, "| $$       \  $$$$/ | $$  | $$|  $$$$$$");
            System.x86.Vga.Put_String (1, 5, Terminal_Foreground,
              Terminal_Background, "| $$        >$$  $$ | $$  | $$ \____  $$");
            System.x86.Vga.Put_String (1, 6, Terminal_Foreground,
              Terminal_Background, "| $$    $$ /$$/\  $$| $$  | $$ /$$  \ $$");
            System.x86.Vga.Put_String (1, 7, Terminal_Foreground,
              Terminal_Background, "|  $$$$$$/| $$  \ $$|  $$$$$$/|  $$$$$$/");
            System.x86.Vga.Put_String (1, 8, Terminal_Foreground,
              Terminal_Background, " \______/ |__/  |__/ \______/  \______/");
         end Print_Vga_Splash;

   end Initialise;
end System.x86.Multiboot;
