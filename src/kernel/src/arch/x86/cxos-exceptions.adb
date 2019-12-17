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

with x86.IDT;
with x86.Serial;

package body Cxos.Exceptions is
   ----------------------------------------------------------------------------
   --  Exception_0_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_0_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 0 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_0_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_10_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_10_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 10 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_10_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_11_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_11_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 11 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_11_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_12_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_12_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 12 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_12_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_13_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_13_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 13 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_13_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_14_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_14_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 14 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_14_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_15_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_15_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 15 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_15_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_16_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_16_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 16 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_16_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_17_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_17_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 17 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_17_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_18_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_18_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 18 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_18_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_19_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_19_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 19 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_19_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_1_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_1_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 1 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_1_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_20_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_20_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 20 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_20_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_21_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_21_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 21 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_21_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_22_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_22_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 22 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_22_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_23_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_23_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 23 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_23_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_24_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_24_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 24 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_24_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_25_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_25_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 25 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_25_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_26_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_26_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 26 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_26_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_27_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_27_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 27 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_27_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_28_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_28_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 28 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_28_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_29_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_29_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 29 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_29_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_2_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_2_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 2 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_2_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_30_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_30_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 30 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_30_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_31_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_31_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 31 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_31_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_3_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_3_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 3 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_3_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_4_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_4_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 4 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_4_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_5_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_5_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 5 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_5_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_6_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_6_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 6 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_6_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_7_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_7_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 7 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_7_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_8_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_8_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 8 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_8_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_9_Internal_Handler
   ----------------------------------------------------------------------------
   procedure Exception_9_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      x86.Serial.Put_String (x86.Serial.COM1,
        "Exception 9 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_9_Internal_Handler;

   ----------------------------------------------------------------------------
   --  Exception_Handler
   ----------------------------------------------------------------------------
   procedure Exception_Handler (
     GS               : Unsigned_32;
     FS               : Unsigned_32;
     ES               : Unsigned_32;
     DS               : Unsigned_32;
     EDI              : Unsigned_32;
     ESI              : Unsigned_32;
     EBP              : Unsigned_32;
     ESP              : Unsigned_32;
     EBX              : Unsigned_32;
     EDX              : Unsigned_32;
     ECX              : Unsigned_32;
     EAX              : Unsigned_32;
     Interrupt_Number : Unsigned_32;
     Error_Code       : Unsigned_32;
     EIP              : Unsigned_32;
     CS               : Unsigned_32;
     EFLAGS           : Unsigned_32;
     USERESP          : Unsigned_32;
     SS               : Unsigned_32
   ) is
      --  Stack frame record to hold the register values pushed
      --  to the stack by the ISR entry function.
      Stack_Frame : Exception_Stack_Frame;
   begin
      --  Populate the stack frame structure.
      Stack_Frame := (
        GS               => GS,
        FS               => FS,
        ES               => ES,
        DS               => DS,
        EDI              => EDI,
        ESI              => ESI,
        EBP              => EBP,
        ESP              => ESP,
        EBX              => EBX,
        EDX              => EDX,
        ECX              => ECX,
        EAX              => EAX,
        Interrupt_Number => Interrupt_Number,
        Error_Code       => Error_Code,
        EIP              => EIP,
        CS               => CS,
        EFLAGS           => EFLAGS,
        USERESP          => USERESP,
        SS               => SS
      );

      --  Pass control to the relevant individual exception handler.
      case Interrupt_Number is
         when 0 =>
            Exception_0_Internal_Handler (Stack_Frame);
         when 1 =>
            Exception_1_Internal_Handler (Stack_Frame);
         when 2 =>
            Exception_2_Internal_Handler (Stack_Frame);
         when 3 =>
            Exception_3_Internal_Handler (Stack_Frame);
         when 4 =>
            Exception_4_Internal_Handler (Stack_Frame);
         when 5 =>
            Exception_5_Internal_Handler (Stack_Frame);
         when 6 =>
            Exception_6_Internal_Handler (Stack_Frame);
         when 7 =>
            Exception_7_Internal_Handler (Stack_Frame);
         when 8 =>
            Exception_8_Internal_Handler (Stack_Frame);
         when 9 =>
            Exception_9_Internal_Handler (Stack_Frame);
         when 10 =>
            Exception_10_Internal_Handler (Stack_Frame);
         when 11 =>
            Exception_11_Internal_Handler (Stack_Frame);
         when 12 =>
            Exception_12_Internal_Handler (Stack_Frame);
         when 13 =>
            Exception_13_Internal_Handler (Stack_Frame);
         when 14 =>
            Exception_14_Internal_Handler (Stack_Frame);
         when 15 =>
            Exception_15_Internal_Handler (Stack_Frame);
         when 16 =>
            Exception_16_Internal_Handler (Stack_Frame);
         when 17 =>
            Exception_17_Internal_Handler (Stack_Frame);
         when 18 =>
            Exception_18_Internal_Handler (Stack_Frame);
         when 19 =>
            Exception_19_Internal_Handler (Stack_Frame);
         when 20 =>
            Exception_20_Internal_Handler (Stack_Frame);
         when 21 =>
            Exception_21_Internal_Handler (Stack_Frame);
         when 22 =>
            Exception_22_Internal_Handler (Stack_Frame);
         when 23 =>
            Exception_23_Internal_Handler (Stack_Frame);
         when 24 =>
            Exception_24_Internal_Handler (Stack_Frame);
         when 25 =>
            Exception_25_Internal_Handler (Stack_Frame);
         when 26 =>
            Exception_26_Internal_Handler (Stack_Frame);
         when 27 =>
            Exception_27_Internal_Handler (Stack_Frame);
         when 28 =>
            Exception_28_Internal_Handler (Stack_Frame);
         when 29 =>
            Exception_29_Internal_Handler (Stack_Frame);
         when 30 =>
            Exception_30_Internal_Handler (Stack_Frame);
         when 31 =>
            Exception_31_Internal_Handler (Stack_Frame);
         when others =>
            x86.Serial.Put_String (x86.Serial.COM1,
              "Unknown Exception occurred. System Halted." & ASCII.LF);
            Halt_Processor;
      end case;

   end Exception_Handler;

   ----------------------------------------------------------------------------
   --  Halt_Processor
   --
   --  Implementation Notes:
   --   - This procedure locks the processor in an endless loop.
   ----------------------------------------------------------------------------
   procedure Halt_Processor is
   begin
      loop
         null;
      end loop;
   end Halt_Processor;

   procedure Initialise is
   begin
      x86.IDT.Install_Descriptor (0,
        Cxos.Exceptions.Exception_0_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (1,
        Cxos.Exceptions.Exception_1_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (2,
        Cxos.Exceptions.Exception_2_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (3,
        Cxos.Exceptions.Exception_3_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (4,
        Cxos.Exceptions.Exception_4_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (5,
        Cxos.Exceptions.Exception_5_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (6,
        Cxos.Exceptions.Exception_6_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (7,
        Cxos.Exceptions.Exception_7_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (8,
        Cxos.Exceptions.Exception_8_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (9,
        Cxos.Exceptions.Exception_9_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (10,
        Cxos.Exceptions.Exception_10_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (11,
        Cxos.Exceptions.Exception_11_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (12,
        Cxos.Exceptions.Exception_12_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (13,
        Cxos.Exceptions.Exception_13_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (14,
        Cxos.Exceptions.Exception_14_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (15,
        Cxos.Exceptions.Exception_15_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (16,
        Cxos.Exceptions.Exception_16_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (17,
        Cxos.Exceptions.Exception_17_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (18,
        Cxos.Exceptions.Exception_18_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (19,
        Cxos.Exceptions.Exception_19_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (20,
        Cxos.Exceptions.Exception_20_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (21,
        Cxos.Exceptions.Exception_21_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (22,
        Cxos.Exceptions.Exception_22_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (23,
        Cxos.Exceptions.Exception_23_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (24,
        Cxos.Exceptions.Exception_24_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (25,
        Cxos.Exceptions.Exception_25_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (26,
        Cxos.Exceptions.Exception_26_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (27,
        Cxos.Exceptions.Exception_27_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (28,
        Cxos.Exceptions.Exception_28_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (29,
        Cxos.Exceptions.Exception_29_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (30,
        Cxos.Exceptions.Exception_30_Handler'Address, 16#8#);

      x86.IDT.Install_Descriptor (31,
        Cxos.Exceptions.Exception_31_Handler'Address, 16#8#);
   end Initialise;

end Cxos.Exceptions;
