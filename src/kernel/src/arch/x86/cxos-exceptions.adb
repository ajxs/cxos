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

with Cxos.Debug;
with x86.IDT;

package body Cxos.Exceptions is
   ----------------------------------------------------------------------------
   --  Exception_0_Handler
   ----------------------------------------------------------------------------
   procedure Exception_0_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 0 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_0_Handler;

   ----------------------------------------------------------------------------
   --  Exception_10_Handler
   ----------------------------------------------------------------------------
   procedure Exception_10_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 10 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_10_Handler;

   ----------------------------------------------------------------------------
   --  Exception_11_Handler
   ----------------------------------------------------------------------------
   procedure Exception_11_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 11 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_11_Handler;

   ----------------------------------------------------------------------------
   --  Exception_12_Handler
   ----------------------------------------------------------------------------
   procedure Exception_12_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 12 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_12_Handler;

   ----------------------------------------------------------------------------
   --  Exception_13_Handler
   ----------------------------------------------------------------------------
   procedure Exception_13_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 13 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_13_Handler;

   ----------------------------------------------------------------------------
   --  Exception_14_Handler
   ----------------------------------------------------------------------------
   procedure Exception_14_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 14 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_14_Handler;

   ----------------------------------------------------------------------------
   --  Exception_15_Handler
   ----------------------------------------------------------------------------
   procedure Exception_15_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 15 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_15_Handler;

   ----------------------------------------------------------------------------
   --  Exception_16_Handler
   ----------------------------------------------------------------------------
   procedure Exception_16_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 16 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_16_Handler;

   ----------------------------------------------------------------------------
   --  Exception_17_Handler
   ----------------------------------------------------------------------------
   procedure Exception_17_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 17 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_17_Handler;

   ----------------------------------------------------------------------------
   --  Exception_18_Handler
   ----------------------------------------------------------------------------
   procedure Exception_18_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 18 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_18_Handler;

   ----------------------------------------------------------------------------
   --  Exception_19_Handler
   ----------------------------------------------------------------------------
   procedure Exception_19_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 19 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_19_Handler;

   ----------------------------------------------------------------------------
   --  Exception_1_Handler
   ----------------------------------------------------------------------------
   procedure Exception_1_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 1 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_1_Handler;

   ----------------------------------------------------------------------------
   --  Exception_20_Handler
   ----------------------------------------------------------------------------
   procedure Exception_20_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 20 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_20_Handler;

   ----------------------------------------------------------------------------
   --  Exception_21_Handler
   ----------------------------------------------------------------------------
   procedure Exception_21_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 21 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_21_Handler;

   ----------------------------------------------------------------------------
   --  Exception_22_Handler
   ----------------------------------------------------------------------------
   procedure Exception_22_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 22 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_22_Handler;

   ----------------------------------------------------------------------------
   --  Exception_23_Handler
   ----------------------------------------------------------------------------
   procedure Exception_23_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 23 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_23_Handler;

   ----------------------------------------------------------------------------
   --  Exception_24_Handler
   ----------------------------------------------------------------------------
   procedure Exception_24_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 24 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_24_Handler;

   ----------------------------------------------------------------------------
   --  Exception_25_Handler
   ----------------------------------------------------------------------------
   procedure Exception_25_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 25 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_25_Handler;

   ----------------------------------------------------------------------------
   --  Exception_26_Handler
   ----------------------------------------------------------------------------
   procedure Exception_26_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 26 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_26_Handler;

   ----------------------------------------------------------------------------
   --  Exception_27_Handler
   ----------------------------------------------------------------------------
   procedure Exception_27_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 27 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_27_Handler;

   ----------------------------------------------------------------------------
   --  Exception_28_Handler
   ----------------------------------------------------------------------------
   procedure Exception_28_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 28 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_28_Handler;

   ----------------------------------------------------------------------------
   --  Exception_29_Handler
   ----------------------------------------------------------------------------
   procedure Exception_29_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 29 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_29_Handler;

   ----------------------------------------------------------------------------
   --  Exception_2_Handler
   ----------------------------------------------------------------------------
   procedure Exception_2_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 2 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_2_Handler;

   ----------------------------------------------------------------------------
   --  Exception_30_Handler
   ----------------------------------------------------------------------------
   procedure Exception_30_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 30 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_30_Handler;

   ----------------------------------------------------------------------------
   --  Exception_31_Handler
   ----------------------------------------------------------------------------
   procedure Exception_31_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 31 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_31_Handler;

   ----------------------------------------------------------------------------
   --  Exception_3_Handler
   ----------------------------------------------------------------------------
   procedure Exception_3_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 3 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_3_Handler;

   ----------------------------------------------------------------------------
   --  Exception_4_Handler
   ----------------------------------------------------------------------------
   procedure Exception_4_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 4 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_4_Handler;

   ----------------------------------------------------------------------------
   --  Exception_5_Handler
   ----------------------------------------------------------------------------
   procedure Exception_5_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 5 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_5_Handler;

   ----------------------------------------------------------------------------
   --  Exception_6_Handler
   ----------------------------------------------------------------------------
   procedure Exception_6_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 6 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_6_Handler;

   ----------------------------------------------------------------------------
   --  Exception_7_Handler
   ----------------------------------------------------------------------------
   procedure Exception_7_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 7 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_7_Handler;

   ----------------------------------------------------------------------------
   --  Exception_8_Handler
   ----------------------------------------------------------------------------
   procedure Exception_8_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 8 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_8_Handler;

   ----------------------------------------------------------------------------
   --  Exception_9_Handler
   ----------------------------------------------------------------------------
   procedure Exception_9_Handler (
     Saved_Registers : Exception_Stack_Frame
   ) is
   begin
      pragma Unreferenced (Saved_Registers);
      Cxos.Debug.Put_String (
        "Exception 9 occurred. System Halted." & ASCII.LF);
      Halt_Processor;
   end Exception_9_Handler;

   ----------------------------------------------------------------------------
   --  Exception_Handler
   --
   --  Implementation Notes:
   --    Sets up the stack frame record from the parameters passed to this
   --    function, then calls the appropriate handler.
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
            Exception_0_Handler (Stack_Frame);
         when 1 =>
            Exception_1_Handler (Stack_Frame);
         when 2 =>
            Exception_2_Handler (Stack_Frame);
         when 3 =>
            Exception_3_Handler (Stack_Frame);
         when 4 =>
            Exception_4_Handler (Stack_Frame);
         when 5 =>
            Exception_5_Handler (Stack_Frame);
         when 6 =>
            Exception_6_Handler (Stack_Frame);
         when 7 =>
            Exception_7_Handler (Stack_Frame);
         when 8 =>
            Exception_8_Handler (Stack_Frame);
         when 9 =>
            Exception_9_Handler (Stack_Frame);
         when 10 =>
            Exception_10_Handler (Stack_Frame);
         when 11 =>
            Exception_11_Handler (Stack_Frame);
         when 12 =>
            Exception_12_Handler (Stack_Frame);
         when 13 =>
            Exception_13_Handler (Stack_Frame);
         when 14 =>
            Exception_14_Handler (Stack_Frame);
         when 15 =>
            Exception_15_Handler (Stack_Frame);
         when 16 =>
            Exception_16_Handler (Stack_Frame);
         when 17 =>
            Exception_17_Handler (Stack_Frame);
         when 18 =>
            Exception_18_Handler (Stack_Frame);
         when 19 =>
            Exception_19_Handler (Stack_Frame);
         when 20 =>
            Exception_20_Handler (Stack_Frame);
         when 21 =>
            Exception_21_Handler (Stack_Frame);
         when 22 =>
            Exception_22_Handler (Stack_Frame);
         when 23 =>
            Exception_23_Handler (Stack_Frame);
         when 24 =>
            Exception_24_Handler (Stack_Frame);
         when 25 =>
            Exception_25_Handler (Stack_Frame);
         when 26 =>
            Exception_26_Handler (Stack_Frame);
         when 27 =>
            Exception_27_Handler (Stack_Frame);
         when 28 =>
            Exception_28_Handler (Stack_Frame);
         when 29 =>
            Exception_29_Handler (Stack_Frame);
         when 30 =>
            Exception_30_Handler (Stack_Frame);
         when 31 =>
            Exception_31_Handler (Stack_Frame);
         when others =>
            Cxos.Debug.Put_String (
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

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      x86.IDT.Install_Descriptor (0,  Exception_0_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (1,  Exception_1_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (2,  Exception_2_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (3,  Exception_3_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (4,  Exception_4_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (5,  Exception_5_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (6,  Exception_6_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (7,  Exception_7_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (8,  Exception_8_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (9,  Exception_9_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (10, Exception_10_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (11, Exception_11_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (12, Exception_12_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (13, Exception_13_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (14, Exception_14_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (15, Exception_15_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (16, Exception_16_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (17, Exception_17_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (18, Exception_18_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (19, Exception_19_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (20, Exception_20_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (21, Exception_21_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (22, Exception_22_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (23, Exception_23_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (24, Exception_24_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (25, Exception_25_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (26, Exception_26_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (27, Exception_27_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (28, Exception_28_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (29, Exception_29_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (30, Exception_30_Entry'Address, 16#8#);
      x86.IDT.Install_Descriptor (31, Exception_31_Entry'Address, 16#8#);
   end Initialise;

end Cxos.Exceptions;
