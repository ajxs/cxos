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

with Interfaces;

-------------------------------------------------------------------------------
--  SYSTEM.X86.EXCEPTIONS
--
--  Purpose:
--    This package contains code for handling x86 processor exceptions.
-------------------------------------------------------------------------------
package x86.Exceptions is
   pragma Preelaborate (x86.Exceptions);

   use Interfaces;

   ----------------------------------------------------------------------------
   --  Exception_0_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 0.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_0_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception0_entry";

   ----------------------------------------------------------------------------
   --  Exception_1_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 1.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_1_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception1_entry";

   ----------------------------------------------------------------------------
   --  Exception_2_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 2.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_2_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception2_entry";

   ----------------------------------------------------------------------------
   --  Exception_3_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 3.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_3_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception3_entry";

   ----------------------------------------------------------------------------
   --  Exception_4_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 4.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_4_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception4_entry";

   ----------------------------------------------------------------------------
   --  Exception_5_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 5.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_5_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception5_entry";

   ----------------------------------------------------------------------------
   --  Exception_6_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 6.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_6_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception6_entry";

   ----------------------------------------------------------------------------
   --  Exception_7_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 7.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_7_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception7_entry";

   ----------------------------------------------------------------------------
   --  Exception_8_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 8.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_8_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception8_entry";

   ----------------------------------------------------------------------------
   --  Exception_9_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 9.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_9_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception9_entry";

   ----------------------------------------------------------------------------
   --  Exception_10_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 10.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_10_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception10_entry";

   ----------------------------------------------------------------------------
   --  Exception_11_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 11.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_11_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception11_entry";

   ----------------------------------------------------------------------------
   --  Exception_12_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 12.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_12_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception12_entry";

   ----------------------------------------------------------------------------
   --  Exception_13_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 13.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_13_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception13_entry";

   ----------------------------------------------------------------------------
   --  Exception_14_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 14.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_14_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception14_entry";

   ----------------------------------------------------------------------------
   --  Exception_15_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 15.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_15_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception15_entry";

   ----------------------------------------------------------------------------
   --  Exception_16_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 16.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_16_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception16_entry";

   ----------------------------------------------------------------------------
   --  Exception_17_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 17.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_17_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception17_entry";

   ----------------------------------------------------------------------------
   --  Exception_18_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 18.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_18_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception18_entry";

   ----------------------------------------------------------------------------
   --  Exception_19_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 19.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_19_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception19_entry";

   ----------------------------------------------------------------------------
   --  Exception_20_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 20.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_20_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception20_entry";

   ----------------------------------------------------------------------------
   --  Exception_21_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 21.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_21_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception21_entry";

   ----------------------------------------------------------------------------
   --  Exception_22_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 22.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_22_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception22_entry";

   ----------------------------------------------------------------------------
   --  Exception_23_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 23.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_23_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception23_entry";

   ----------------------------------------------------------------------------
   --  Exception_24_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 24.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_24_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception24_entry";

   ----------------------------------------------------------------------------
   --  Exception_25_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 25.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_25_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception25_entry";

   ----------------------------------------------------------------------------
   --  Exception_26_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 26.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_26_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception26_entry";

   ----------------------------------------------------------------------------
   --  Exception_27_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 27.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_27_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception27_entry";

   ----------------------------------------------------------------------------
   --  Exception_28_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 28.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_28_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception28_entry";

   ----------------------------------------------------------------------------
   --  Exception_29_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 29.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_29_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception29_entry";

   ----------------------------------------------------------------------------
   --  Exception_30_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 30.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_30_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception30_entry";

   ----------------------------------------------------------------------------
   --  Exception_31_Handler
   --
   --  Purpose:
   --    This procedure handles processor exception 30.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_31_Handler
   with Import,
     Convention    => Assembler,
     External_Name => "__exception31_entry";

private
   ----------------------------------------------------------------------------
   --  Exception Stack Frame Type
   --  Record structure containing the saved registers pushed to the stack
   --  by the ISR entry handler function.
   ----------------------------------------------------------------------------
   type Exception_Stack_Frame is
      record
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
         SS               : Unsigned_32;
      end record;

   ----------------------------------------------------------------------------
   --  Exception_Handler
   --
   --  Purpose:
   --    This procedure serves as a common entry point to all of the internal
   --    exception handler functions. Its main purpose is to parse the stack
   --    frame passed to us by the ISR entry handler function and then call
   --    individual specific exception handlers.
   --  Exceptions:
   --    None.
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
   )
   with Export,
     Convention    => Assembler,
     External_Name => "__exception_handler";

   ----------------------------------------------------------------------------
   --  Exception_0_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 0.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_0_Internal_Handler (
     Saved_Registers : Exception_Stack_Frame
   )
   with Export     => True,
     Convention    => C,
     External_Name => "__exception0_handler";

   ----------------------------------------------------------------------------
   --  Exception_1_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 1.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_1_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception1_handler";

   ----------------------------------------------------------------------------
   --  Exception_2_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 2.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_2_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception2_handler";

   ----------------------------------------------------------------------------
   --  Exception_3_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 3.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_3_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception3_handler";

   ----------------------------------------------------------------------------
   --  Exception_4_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 4.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_4_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception4_handler";

   ----------------------------------------------------------------------------
   --  Exception_5_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 5.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_5_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception5_handler";

   ----------------------------------------------------------------------------
   --  Exception_6_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 6.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_6_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception6_handler";

   ----------------------------------------------------------------------------
   --  Exception_7_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 7.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_7_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception7_handler";

   ----------------------------------------------------------------------------
   --  Exception_8_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 8.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_8_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception8_handler";

   ----------------------------------------------------------------------------
   --  Exception_9_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 9.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_9_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception9_handler";

   ----------------------------------------------------------------------------
   --  Exception_10_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 10.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_10_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception10_handler";

   ----------------------------------------------------------------------------
   --  Exception_11_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 11.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_11_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception11_handler";

   ----------------------------------------------------------------------------
   --  Exception_12_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 12.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_12_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception12_handler";

   ----------------------------------------------------------------------------
   --  Exception_13_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 13.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_13_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception13_handler";

   ----------------------------------------------------------------------------
   --  Exception_14_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 14.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_14_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception14_handler";

   ----------------------------------------------------------------------------
   --  Exception_15_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 15.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_15_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception15_handler";

   ----------------------------------------------------------------------------
   --  Exception_16_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 16.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_16_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception16_handler";

   ----------------------------------------------------------------------------
   --  Exception_17_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 17.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_17_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception17_handler";

   ----------------------------------------------------------------------------
   --  Exception_18_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 18.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_18_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception18_handler";

   ----------------------------------------------------------------------------
   --  Exception_19_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 19.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_19_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception19_handler";

   ----------------------------------------------------------------------------
   --  Exception_20_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 20.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_20_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception20_handler";

   ----------------------------------------------------------------------------
   --  Exception_21_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 21.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_21_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception21_handler";

   ----------------------------------------------------------------------------
   --  Exception_22_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 22.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_22_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception22_handler";

   ----------------------------------------------------------------------------
   --  Exception_23_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 23.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_23_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception23_handler";

   ----------------------------------------------------------------------------
   --  Exception_24_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 24.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_24_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception24_handler";

   ----------------------------------------------------------------------------
   --  Exception_25_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 25.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_25_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception25_handler";

   ----------------------------------------------------------------------------
   --  Exception_26_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 26.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_26_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception26_handler";

   ----------------------------------------------------------------------------
   --  Exception_27_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 27.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_27_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception27_handler";

   ----------------------------------------------------------------------------
   --  Exception_28_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 28.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_28_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception28_handler";

   ----------------------------------------------------------------------------
   --  Exception_29_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 29.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_29_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception29_handler";

   ----------------------------------------------------------------------------
   --  Exception_30_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 30.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_30_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception30_handler";

   ----------------------------------------------------------------------------
   --  Exception_31_Internal_Handler
   --
   --  Purpose:
   --    This procedure contains the Ada code for handling exception 31.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Exception_31_Internal_Handler
   with Export,
     Convention    => Assembler,
     External_Name => "__exception31_handler";

   ----------------------------------------------------------------------------
   --  Halt_Processor
   --
   --  Purpose:
   --    This procedure halts the processor in the event of an exception.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Halt_Processor
   with No_Return;

end x86.Exceptions;
