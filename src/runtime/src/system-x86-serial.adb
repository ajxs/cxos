with System.x86.Port_IO;
with System.Storage_Elements;

package body System.x86.Serial is
   use System.Storage_Elements;

   ----------------------------------------------------------------------------
   --  Get_Port_Address
   --
   --  Implementation Notes:
   --   - Returns a constant value stored within the function.
   ----------------------------------------------------------------------------
   function Get_Port_Address (
     Port : Serial_Port
   ) return System.Address is
      COM1_Address : constant System.Address := To_Address (16#3F8#);
      COM2_Address : constant System.Address := To_Address (16#3F8#);
      COM3_Address : constant System.Address := To_Address (16#3F8#);
      COM4_Address : constant System.Address := To_Address (16#3F8#);
   begin
      case Port is
         when COM1 =>
            return COM1_Address;
         when COM2 =>
            return COM2_Address;
         when COM3 =>
            return COM3_Address;
         when COM4 =>
            return COM4_Address;
      end case;
   exception
      when Constraint_Error =>
         return COM1_Address;
   end Get_Port_Address;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Implementation Notes:
   --   - All interrupts are disabled during initialisation.
   ----------------------------------------------------------------------------
   procedure Initialise (
     Port : Serial_Port;
     Rate : Baud_Rate := MAXIMUM_BAUD_RATE
   ) is
      Port_Address : System.Address;
   begin
      --  Get the address for the selected serial port.
      Get_COM_Port_Address :
         begin
            Port_Address := Get_Port_Address (Port);
         exception
            when Constraint_Error =>
               return;
         end Get_COM_Port_Address;

      --  Disable all interrupts.
      System.x86.Port_IO.Outb (Port_Address + 1, 0);

      --  Set the baud rate.
      Set_Baud_Rate (Port, Rate);

      --  Configure the port with 8 bit word length.
      --  No parity bit, one stop bit.
      System.x86.Port_IO.Outb (Port_Address + 3, 16#03#);

      --  Enable FIFO.
      System.x86.Port_IO.Outb (Port_Address + 2, 16#C7#);

   exception
      when Constraint_Error =>
         null;
   end Initialise;

   ----------------------------------------------------------------------------
   --  Is_Tx_Empty
   --
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   function Is_Tx_Empty (
     Port : Serial_Port
   ) return Boolean is
      --  The port mapped address for this COM port.
      Port_Address : System.Address;
   begin
      --  Get the address for the selected serial port.
      Get_COM_Port_Address :
         begin
            Port_Address := Get_Port_Address (Port);
         exception
            when Constraint_Error =>
               return False;
         end Get_COM_Port_Address;

      return (System.x86.Port_IO.Inb (Port_Address + 5)  and 16#20#) /= 0;
   end Is_Tx_Empty;

   ----------------------------------------------------------------------------
   --  Put_Char
   --
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Put_Char (
     Port : Serial_Port;
     Data : Unsigned_8
   ) is
      --  The port mapped address for this COM port.
      Port_Address : System.Address;
   begin
      --  Get the address for the selected serial port.
      Get_COM_Port_Address :
         begin
            Port_Address := Get_Port_Address (Port);
         exception
            when Constraint_Error =>
               return;
         end Get_COM_Port_Address;

      while Is_Tx_Empty (Port) = False loop
         null;
      end loop;

      System.x86.Port_IO.Outb (Port_Address, Data);
   exception
      when Constraint_Error =>
         null;
   end Put_Char;

   ----------------------------------------------------------------------------
   --  Put_String
   --
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Put_String (
     Port : Serial_Port;
     Data : String
   ) is
   begin
      Print_Loop :
         for C of Data loop
            Put_Char (Port, Character'Pos (C));
         end loop Print_Loop;
   exception
      when Constraint_Error =>
         null;
   end Put_String;

   ----------------------------------------------------------------------------
   --  Set_Baud_Rate
   --
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Set_Baud_Rate (
     Port : Serial_Port;
     Rate : Baud_Rate
   ) is
      --  The baud rate divisor for this baud rate.
      Divisor : Unsigned_16;
      --  The value to write into the divisor low register.
      Divisor_Low_Byte : Unsigned_8;
      --  The value to write into the divisor high register.
      Divisor_High_Byte : Unsigned_8;
      --  The port mapped address for this COM port.
      Port_Address : System.Address;
   begin
      --  Get the address for the selected serial port.
      Get_COM_Port_Address :
         begin
            Port_Address := Get_Port_Address (Port);
         exception
            when Constraint_Error =>
               return;
         end Get_COM_Port_Address;

      Get_Divisor :
         begin
            Divisor := Unsigned_16 (MAXIMUM_BAUD_RATE / Rate);
         exception
            --  If an invalid value is generated, set the divisor to 1.
            when Constraint_Error =>
               Divisor := 1;
         end Get_Divisor;

      Get_Divisor_Registers :
         begin
            Divisor_Low_Byte := Unsigned_8 (Divisor and 16#FF#);
            Divisor_High_Byte :=
            Unsigned_8 (Shift_Right (Divisor, 8) and 16#FF#);
         exception
            --  In the case of any errors here, default to the
            --  highest baud rate setting.
            when Constraint_Error =>
               Divisor_Low_Byte := 16#01#;
               Divisor_High_Byte := 16#0#;
         end Get_Divisor_Registers;

      --  Enable DLAB.
      Set_Divisor_Latch_State (Port, True);
      --  Set baud rate divisor low byte to 3 38400 baud.
      System.x86.Port_IO.Outb (Port_Address + 0, Divisor_Low_Byte);
      --  Set baud rate divisor high byte.
      System.x86.Port_IO.Outb (Port_Address + 1, Divisor_High_Byte);
      --  Disable DLAB.
      Set_Divisor_Latch_State (Port, False);
   exception
      when Constraint_Error =>
         null;
   end Set_Baud_Rate;

   ----------------------------------------------------------------------------
   --  Set_Divisor_Latch_State
   --
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Set_Divisor_Latch_State (
     Port  : Serial_Port;
     State : Boolean
   ) is
      --  The existing line map status value.
      Line_Control_Status : Unsigned_8;
      --  The port mapped address for this COM port.
      Port_Address : System.Address;
   begin
      --  Get the address for the selected serial port.
      Get_COM_Port_Address :
         begin
            Port_Address := Get_Port_Address (Port);
         exception
            when Constraint_Error =>
               return;
         end Get_COM_Port_Address;

      --  Get the existing line control status, and modify accordingly
      --  to set the divisor latch state.
      Line_Control_Status := System.x86.Port_IO.Inb (Port_Address + 3);

      case State is
         when True =>
            Line_Control_Status := Line_Control_Status or 16#80#;
         when False =>
            Line_Control_Status := Line_Control_Status and (not 16#80#);
      end case;

      --  Write the DLAB state.
      System.x86.Port_IO.Outb (Port_Address + 3, Line_Control_Status);
   exception
      when Constraint_Error =>
         null;
   end Set_Divisor_Latch_State;

   ----------------------------------------------------------------------------
   --  Set_Interrupt_Generation
   --
   --  Implementation Notes:
   --   - Does not determine whether the port has been initialised.
   ----------------------------------------------------------------------------
   procedure Set_Interrupt_Generation (
     Port           : Serial_Port;
     Interrupt_Type : Serial_Interrupt_Type;
     Status         : Boolean
   ) is
      Interrupt_Status : Unsigned_8;
      Port_Address     : System.Address;
   begin
      --  Get the address for the selected serial port.
      Get_COM_Port_Address :
         begin
            Port_Address := Get_Port_Address (Port);
         exception
            when Constraint_Error =>
               return;
         end Get_COM_Port_Address;

      --  Get the current status of this device's interrupts to
      --  preserve the current interrupt status.
      Get_Interrupt_Status :
         begin
            Interrupt_Status := System.x86.Port_IO.Inb (Port_Address + 1);
         end Get_Interrupt_Status;

      Set_Interrupt_Status :
         begin
            --  Set the interrupt status var to the desired value.
            case Interrupt_Type is
               when Modem_Line_Status =>
                  if Status then
                     Interrupt_Status := Interrupt_Status or 16#8#;
                  else
                     Interrupt_Status := Interrupt_Status and (not 16#8#);
                  end if;
               when Rx_Data_Available =>
                  if Status then
                     Interrupt_Status := Interrupt_Status or 16#1#;
                  else
                     Interrupt_Status := Interrupt_Status and (not 16#1#);
                  end if;
               when Rx_Line_Status =>
                  if Status then
                     Interrupt_Status := Interrupt_Status or 16#4#;
                  else
                     Interrupt_Status := Interrupt_Status and (not 16#4#);
                  end if;
               when Tx_Empty =>
                  if Status then
                     Interrupt_Status := Interrupt_Status or 16#2#;
                  else
                     Interrupt_Status := Interrupt_Status and (not 16#2#);
                  end if;
            end case;
         exception
            when Constraint_Error =>
               return;
         end Set_Interrupt_Status;

      --  Write to the Interrupt enable register.
      System.x86.Port_IO.Outb (Port_Address + 1, Interrupt_Status);
   end Set_Interrupt_Generation;
end System.x86.Serial;
