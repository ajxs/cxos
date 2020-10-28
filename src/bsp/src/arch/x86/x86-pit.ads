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

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;

-------------------------------------------------------------------------------
--  X86.PIT
--
--  Purpose:
--    This package contains code for working with the x86 8253/8254
--    Programmable Interval Timer.
-------------------------------------------------------------------------------
package x86.PIT is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --  PIT Channel type.
   --  Used in selecting which PIT Channel to perform an operation on.
   ----------------------------------------------------------------------------
   type PIT_Channel_T is (
     Channel_0,
     Channel_1,
     Channel_2
   );

   ----------------------------------------------------------------------------
   --  PIT register type.
   --  Used in selecting which PIT Channel to perform an operation on.
   ----------------------------------------------------------------------------
   type PIT_Register_T is (
     Channel_0_Data,
     Channel_1_Data,
     Channel_2_Data,
     Command
   );

   ----------------------------------------------------------------------------
   --  Get_Register_Address
   --
   --  Purpose:
   --    This procedure returns the address of one of the internal
   --    PIT configuration registers.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Get_Register_Address (
     Register : PIT_Register_T
   ) return System.Address
   with Pure_Function;

   ----------------------------------------------------------------------------
   --  PIT Channel select type.
   ----------------------------------------------------------------------------
   type Channel_Select_Type is (
     Channel_0,
     Channel_1,
     Channel_2,
     Read_Back_Command
   )
   with Size => 2;
   for Channel_Select_Type use (
     Channel_0         => 2#00#,
     Channel_1         => 2#01#,
     Channel_2         => 2#10#,
     Read_Back_Command => 2#11#
   );

   ----------------------------------------------------------------------------
   --  PIT Access type.
   ----------------------------------------------------------------------------
   type Port_Access_Type is (
     Latch_Count,
     Low_Byte_Only,
     High_Byte_Only,
     Low_High_Byte
   )
   with Size => 2;
   for Port_Access_Type use (
     Latch_Count    => 2#00#,
     Low_Byte_Only  => 2#01#,
     High_Byte_Only => 2#10#,
     Low_High_Byte  => 2#11#
   );

   ----------------------------------------------------------------------------
   --  PIT Operating mode type.
   ----------------------------------------------------------------------------
   type Operating_Mode_Type is (
     Interrupt_On_Terminal,
     One_Shot,
     Rate_Generator,
     Square_Wave_Generator,
     Software_Triggered_Strobe,
     Hardware_Triggered_Strobe
   )
   with Size => 3;
   for Operating_Mode_Type use (
     Interrupt_On_Terminal     => 2#000#,
     One_Shot                  => 2#001#,
     Rate_Generator            => 2#010#,
     Square_Wave_Generator     => 2#011#,
     Software_Triggered_Strobe => 2#100#,
     Hardware_Triggered_Strobe => 2#101#
   );

   ----------------------------------------------------------------------------
   --  PIT Binary mode type.
   ----------------------------------------------------------------------------
   type Binary_Mode_Type is (
      Binary_16_Bit,
      BCD
   )
   with Size => 1;
   for Binary_Mode_Type use (
      Binary_16_Bit => 0,
      BCD           => 1
   );

   ----------------------------------------------------------------------------
   --  PIT Mode select register type.
   ----------------------------------------------------------------------------
   type Mode_Select_Register is
      record
         Binary_Mode    : Binary_Mode_Type;
         Operating_Mode : Operating_Mode_Type;
         Port_Access    : Port_Access_Type;
         Channel_Select : Channel_Select_Type;
      end record
   with Size => 8;
   for Mode_Select_Register use
      record
         Binary_Mode    at 0 range 0 .. 0;
         Operating_Mode at 0 range 1 .. 3;
         Port_Access    at 0 range 4 .. 5;
         Channel_Select at 0 range 6 .. 7;
      end record;

   ----------------------------------------------------------------------------
   --  Mode_Select_Register_To_Byte
   --
   --  Purpose:
   --    Unchecked conversion to write mode select information to
   --    an IO port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Mode_Select_Register_To_Byte is
      new Ada.Unchecked_Conversion (
        Source => Mode_Select_Register,
        Target => Unsigned_8
      );

   ----------------------------------------------------------------------------
   --  Byte_To_Mode_Select_Register
   --
   --  Purpose:
   --    Unchecked conversion to read mode select information from
   --    an IO port.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Byte_To_Mode_Select_Register is
      new Ada.Unchecked_Conversion (
        Source => Unsigned_8,
        Target => Mode_Select_Register
      );

end x86.PIT;
