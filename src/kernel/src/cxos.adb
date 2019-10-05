with System.Machine_Code;
with x86;
with System.x86.Serial;
with System.x86.Vga;

package body Cxos is
   ----------------------------------------------------------------------------
   --  Main
   ----------------------------------------------------------------------------
   procedure Main is
      use type System.x86.Vga.Color;

      Terminal_Foreground : constant System.x86.Vga.Color := System.x86.Vga.Light_Green;
      Terminal_Background : constant System.x86.Vga.Color := System.x86.Vga.Black;
   begin
      System.x86.Vga.Clear (Terminal_Background);

      --  Print ASCII art test screen.
      System.x86.Vga.Put_String (1, 1, Terminal_Foreground, Terminal_Background,
        "  /$$$$$$  /$$   /$$  /$$$$$$   /$$$$$$ ");
      System.x86.Vga.Put_String (1, 2, Terminal_Foreground, Terminal_Background,
        " /$$__  $$| $$  / $$ /$$__  $$ /$$__  $$");
      System.x86.Vga.Put_String (1, 3, Terminal_Foreground, Terminal_Background,
        "| $$  \__/|  $$/ $$/| $$  \ $$| $$  \__/");
      System.x86.Vga.Put_String (1, 4, Terminal_Foreground, Terminal_Background,
        "| $$       \  $$$$/ | $$  | $$|  $$$$$$");
      System.x86.Vga.Put_String (1, 5, Terminal_Foreground, Terminal_Background,
        "| $$        >$$  $$ | $$  | $$ \____  $$");
      System.x86.Vga.Put_String (1, 6, Terminal_Foreground, Terminal_Background,
        "| $$    $$ /$$/\  $$| $$  | $$ /$$  \ $$");
      System.x86.Vga.Put_String (1, 7, Terminal_Foreground, Terminal_Background,
        "|  $$$$$$/| $$  \ $$|  $$$$$$/|  $$$$$$/");
      System.x86.Vga.Put_String (1, 8, Terminal_Foreground, Terminal_Background,
        " \______/ |__/  |__/ \______/  \______/");

      System.x86.Serial.Put_String (System.x86.Serial.COM1,
        "Hello Serial world!" & ASCII.LF);

      --  Loop forever.
      loop
         System.Machine_Code.Asm ("hlt", Volatile => True);
      end loop;
   end Main;
end Cxos;
