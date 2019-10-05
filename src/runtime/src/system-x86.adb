with Ada.Interrupts.Names;
with System.x86.Exceptions;
with System.x86.IDT;
with System.x86.Interrupts;
with System.x86.IRQ_Handlers;
with System.x86.GDT;
with System.x86.PIC;
with System.x86.Serial;

package body System.x86 is
   use Ada.Interrupts.Names;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
   begin
      System.x86.Serial.Initialise (System.x86.Serial.COM1, 38400);
      System.x86.Serial.Put_String (System.x86.Serial.COM1, "COM1 initialised" & ASCII.LF);

      System.x86.Serial.Put_String (System.x86.Serial.COM1, "Initialising PIC" & ASCII.LF);
      System.x86.PIC.Initialise;

      --  Clear interrupts.
      System.x86.Interrupts.Set_Interrupt_Flag (False);

      System.x86.Serial.Put_String (System.x86.Serial.COM1, "Initialising GDT" & ASCII.LF);
      System.x86.GDT.Initialise;

      System.x86.Serial.Put_String (System.x86.Serial.COM1, "Initialising IDT" & ASCII.LF);
      System.x86.IDT.Initialise;

      System.x86.Serial.Put_String (System.x86.Serial.COM1,
        "Installing processor exception handlers" & ASCII.LF);
      Install_Exception_Handlers;

      --  Install a handler for IRQ0.
      System.x86.PIC.Set_Interrupt_Mask (IRQ0, False);
      System.x86.IDT.Install_Descriptor (32,
        System.x86.IRQ_Handlers.IRQ0_Handler'Address, 16#8#);

      --  Install a handler for IRQ1.
      System.x86.PIC.Set_Interrupt_Mask (IRQ1, False);
      System.x86.IDT.Install_Descriptor (33,
        System.x86.IRQ_Handlers.IRQ1_Handler'Address, 16#8#);

      System.x86.IDT.Finalise;

      System.x86.Serial.Put_String (System.x86.Serial.COM1, "Flushing GDT" & ASCII.LF);
      System.x86.GDT.Finalise;

      System.x86.Serial.Put_String (System.x86.Serial.COM1,
        "Jumping to protected mode" & ASCII.LF);
      Protected_Mode_Init;

      --  Enable interrupts.
      System.x86.Interrupts.Set_Interrupt_Flag (True);

      System.x86.Serial.Put_String (System.x86.Serial.COM1,
        "Protected mode entered" & ASCII.LF);

   end Initialise;

   ----------------------------------------------------------------------------
   --  Install_Exception_Handlers
   ----------------------------------------------------------------------------
   procedure Install_Exception_Handlers is
   begin
      System.x86.IDT.Install_Descriptor (0,
        System.x86.Exceptions.Exception_0_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (1,
        System.x86.Exceptions.Exception_1_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (2,
        System.x86.Exceptions.Exception_2_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (3,
        System.x86.Exceptions.Exception_3_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (4,
        System.x86.Exceptions.Exception_4_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (5,
        System.x86.Exceptions.Exception_5_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (6,
        System.x86.Exceptions.Exception_6_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (7,
        System.x86.Exceptions.Exception_7_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (8,
        System.x86.Exceptions.Exception_8_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (9,
        System.x86.Exceptions.Exception_9_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (10,
        System.x86.Exceptions.Exception_10_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (11,
        System.x86.Exceptions.Exception_11_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (12,
        System.x86.Exceptions.Exception_12_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (13,
        System.x86.Exceptions.Exception_13_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (14,
        System.x86.Exceptions.Exception_14_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (15,
        System.x86.Exceptions.Exception_15_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (16,
        System.x86.Exceptions.Exception_16_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (17,
        System.x86.Exceptions.Exception_17_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (18,
        System.x86.Exceptions.Exception_18_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (19,
        System.x86.Exceptions.Exception_19_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (20,
        System.x86.Exceptions.Exception_20_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (21,
        System.x86.Exceptions.Exception_21_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (22,
        System.x86.Exceptions.Exception_22_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (23,
        System.x86.Exceptions.Exception_23_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (24,
        System.x86.Exceptions.Exception_24_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (25,
        System.x86.Exceptions.Exception_25_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (26,
        System.x86.Exceptions.Exception_26_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (27,
        System.x86.Exceptions.Exception_27_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (28,
        System.x86.Exceptions.Exception_28_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (29,
        System.x86.Exceptions.Exception_29_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (30,
        System.x86.Exceptions.Exception_30_Handler'Address, 16#8#);

      System.x86.IDT.Install_Descriptor (31,
        System.x86.Exceptions.Exception_31_Handler'Address, 16#8#);

   end Install_Exception_Handlers;

   ----------------------------------------------------------------------------
   --  Last_Chance_Handler
   ----------------------------------------------------------------------------
   procedure Last_Chance_Handler (
     Msg  : System.Address;
     Line : Integer
   ) is
   begin
      null;
   end Last_Chance_Handler;
end System.x86;
