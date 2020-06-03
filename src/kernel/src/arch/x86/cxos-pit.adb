-------------------------------------------------------------------------------
--  Copyright (c) 2019, CXOS.
--  This program is free software; you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3 of the License, or
--  (at your option) any later version.
--
--  Authors:
--     Anthony <ajxs [at] panoptic.online>
-------------------------------------------------------------------------------

with System;
with x86.Port_IO; use x86.Port_IO;

package body Cxos.PIT is
   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Implementation Notes:
   --   - Only initialises Channel 0.
   ----------------------------------------------------------------------------
   procedure Initialise is
      --  The predefined initial value to select for a 100hz rate.
      INITIAL_VALUE_100HZ : constant Unsigned_16 := 11931;
   begin
      --  Initialise Channel 0 to pulse at 100hz, using operating mode 2,
      --  raising an interrupt every 10ms.
      Initialise_Channel (Channel_0, Rate_Generator, INITIAL_VALUE_100HZ);
   end Initialise;

   ----------------------------------------------------------------------------
   --  Initialise_Channel
   --
   --  Implementation Notes:
   --   - Ensure that interrupt IRQ0 is masked before setting this to avoid
   --     any race conditions in different threads, if applicable.
   ----------------------------------------------------------------------------
   procedure Initialise_Channel (
     Channel        : PIT_Channel;
     Operating_Mode : Operating_Mode_Type;
     Initial_Value  : Unsigned_16
   ) is
      Channel_Address : System.Address;
      Command_Address : System.Address;
      Command_Data    : Mode_Select_Register;
   begin
      if not Operating_Mode'Valid then
         return;
      end if;

      --  Get the register address for the selected PIT channel.
      Get_Channel_Register_Address :
         declare
            Channel_Register : PIT_Register;
         begin
            case Channel is
               when Channel_0 =>
                  Channel_Register := Channel_0_Data;
               when Channel_1 =>
                  Channel_Register := Channel_1_Data;
               when Channel_2 =>
                  Channel_Register := Channel_2_Data;
            end case;

            Command_Address := Get_Register_Address (Command);
            Channel_Address := Get_Register_Address (Channel_Register);
         exception
            when Constraint_Error =>
               return;
         end Get_Channel_Register_Address;

      --  Select the channel the command is for.
      Set_Command_Channel :
         begin
            case Channel is
               when Channel_0 =>
                  Command_Data.Channel_Select := Channel_0;
               when Channel_1 =>
                  Command_Data.Channel_Select := Channel_1;
               when Channel_2 =>
                  Command_Data.Channel_Select := Channel_2;
            end case;
         exception
            when Constraint_Error =>
               return;
         end Set_Command_Channel;

      --  Set the channel operating mode.
      Set_Operating_Mode :
         begin
            Command_Data.Operating_Mode := Operating_Mode;
         exception
            when Constraint_Error =>
               return;
         end Set_Operating_Mode;

      --  Binary mode is always 16bit in x86.
      Command_Data.Binary_Mode := Binary_16_Bit;

      --  Set the access value to prepare the PIT to receive the data.
      Command_Data.Port_Access := Low_High_Byte;

      --  Write the command data to prepare the channel to receive the
      --  low/high bytes of the initial value.
      Outb (Command_Address, Mode_Select_Register_To_Byte (Command_Data));

      --  Write the initial value, low byte first.
      Write_Initial_Value :
         declare
            Low_Byte  : Unsigned_8;
            High_Byte : Unsigned_8;
         begin
            Low_Byte  := Unsigned_8 (Initial_Value and 16#FF#);
            High_Byte := Unsigned_8 (
              Shift_Left (Initial_Value and 16#FF00#, 8));

            Outb (Channel_Address, Low_Byte);
            Outb (Channel_Address, High_Byte);
         exception
            when Constraint_Error =>
               return;
         end Write_Initial_Value;
   end Initialise_Channel;
end Cxos.PIT;
