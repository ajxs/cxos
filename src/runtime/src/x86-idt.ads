with Interfaces;
with System;
with x86.Descriptors;

-------------------------------------------------------------------------------
--  X86.IDT
--
--  Purpose:
--    This package contains code for initialising the Interrupt Descriptor
--    Table. The initialisation procedure within is called by the system
--    init code.
-------------------------------------------------------------------------------
package x86.IDT is
   pragma Preelaborate (x86.IDT);

   use Interfaces;
   use x86.Descriptors;

   ----------------------------------------------------------------------------
   --  Initialise
   --
   --  Purpose:
   --    This procedure initialises the x86 platform's Interrupt
   --    Descriptor Table.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Initialise;

private
   ----------------------------------------------------------------------------
   --  Install_Descriptor
   --
   --  Purpose:
   --    This procedure creates an individual descriptor entry in the x86
   --    platform's Interrupt Descriptor Table.
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   procedure Install_Descriptor (
     Index       : in Descriptor_Entry_Range;
     Offset_Addr : in System.Address;
     Selector    : in Descriptor_Entry_Range;
     Privilege   : in Descriptor_Privilege_Level := Ring_0
   );

   ----------------------------------------------------------------------------
   --  Descriptor type information.
   --  Differs from GDT descriptor type field.
   --  Refer to Page 197. Intel IA-32 SDM 3a.
   ----------------------------------------------------------------------------
   type Descriptor_Type is (
     Interrupt_Gate_16_Bit,
     Interrupt_Gate_32_Bit
   )
   with Size => 4;
   for Descriptor_Type use (
     Interrupt_Gate_16_Bit => 16#6#,
     Interrupt_Gate_32_Bit => 16#E#
   );

   ----------------------------------------------------------------------------
   --  An individual segment descriptor within the IDT.
   --  Refer to Page 197. Intel IA-32 SDM 3a.
   ----------------------------------------------------------------------------
   type IDT_Descriptor is
      record
         Offset_Low  : Unsigned_16;
         Selector    : Unsigned_16;
         Reserved    : Unsigned_8;
         Descr_Type  : Descriptor_Type;
         S           : Boolean;
         DPL         : Descriptor_Privilege_Level;
         P           : Boolean;
         Offset_High : Unsigned_16;
      end record
   with Size => 64;
   for IDT_Descriptor use
      record
         Offset_Low  at 0 range 0  .. 15;
         Selector    at 0 range 16 .. 31;
         Reserved    at 4 range 0  .. 7;
         Descr_Type  at 4 range 8  .. 11;
         S           at 4 range 12 .. 12;
         DPL         at 4 range 13 .. 14;
         P           at 4 range 15 .. 15;
         Offset_High at 4 range 16 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  The format of the IDT pointer needed by the processor to load the IDT.
   ----------------------------------------------------------------------------
   type IDT_Pointer is
      record
         Size   : Unsigned_16;
         Offset : System.Address;
      end record
   with Size => 48;
   for IDT_Pointer use
      record
         Size   at 0 range 0  .. 15;
         Offset at 0 range 16 .. 47;
      end record;

   type IDT_Table is array (Descriptor_Entry_Range range <>) of IDT_Descriptor;

   IDT_LENGTH : constant := 256;

   ----------------------------------------------------------------------------
   --  The actual Interrupt descriptor table entity.
   --  The length of the entries is statically allocated.
   ----------------------------------------------------------------------------
   Interrupt_Descriptor_Table : IDT_Table (0 .. (IDT_LENGTH - 1))
   with Export,
     Convention    => C,
     External_Name => "interrupt_descriptor_table",
     Volatile;

   ----------------------------------------------------------------------------
   --  The pointer to the IDT needed by the processor to load the IDT.
   ----------------------------------------------------------------------------
   IDT_Ptr : IDT_Pointer
   with Export,
     Convention    => C,
     External_Name => "IDT_Pointer",
     Volatile;
end x86.IDT;
