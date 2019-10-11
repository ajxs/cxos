with Cxos.Serial;
with System.Machine_Code;

package body Cxos is
   ----------------------------------------------------------------------------
   --  Main
   ----------------------------------------------------------------------------
   procedure Main is
   begin
      --  Print the ASCII splash screen.
      Print_Splash;

      --  Loop forever.
      loop
         System.Machine_Code.Asm ("hlt", Volatile => True);
      end loop;
   end Main;

   ----------------------------------------------------------------------------
   --  Print_Splash
   ----------------------------------------------------------------------------
   procedure Print_Splash is
      Line_1 : constant String := "  /$$$$$$  /$$   /$$  /$$$$$$   /$$$$$$ ";
      Line_2 : constant String := " /$$__  $$| $$  / $$ /$$__  $$ /$$__  $$";
      Line_3 : constant String := "| $$  \__/|  $$/ $$/| $$  \ $$| $$  \__/";
      Line_4 : constant String := "| $$       \  $$$$/ | $$  | $$|  $$$$$$ ";
      Line_5 : constant String := "| $$        >$$  $$ | $$  | $$ \____  $$";
      Line_6 : constant String := "| $$    $$ /$$/\  $$| $$  | $$ /$$  \ $$";
      Line_7 : constant String := "|  $$$$$$/| $$  \ $$|  $$$$$$/|  $$$$$$/";
      Line_8 : constant String := " \______/ |__/  |__/ \______/  \______/ ";
   begin
      Print_Splash_to_Serial :
         begin
            Cxos.Serial.Put_String ("" & ASCII.LF);
            Cxos.Serial.Put_String (Line_1 & ASCII.LF);
            Cxos.Serial.Put_String (Line_2 & ASCII.LF);
            Cxos.Serial.Put_String (Line_3 & ASCII.LF);
            Cxos.Serial.Put_String (Line_4 & ASCII.LF);
            Cxos.Serial.Put_String (Line_5 & ASCII.LF);
            Cxos.Serial.Put_String (Line_6 & ASCII.LF);
            Cxos.Serial.Put_String (Line_7 & ASCII.LF);
            Cxos.Serial.Put_String (Line_8 & ASCII.LF);
            Cxos.Serial.Put_String ("" & ASCII.LF);
         end Print_Splash_to_Serial;
   end Print_Splash;
end Cxos;
