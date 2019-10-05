with Interfaces;

package System.x86.Descriptors is
   pragma Preelaborate (System.x86.Descriptors);

   use Interfaces;

   ----------------------------------------------------------------------------
   --  The privilege level for a particular descriptor.
   --  These correspond to the 'protection ring' that this descriptor is
   --  accessible from.
   ----------------------------------------------------------------------------
   type Descriptor_Privilege_Level is (
     Ring_0,
     Ring_1,
     Ring_2,
     Ring_3
   )
   with Size => 2;
   for Descriptor_Privilege_Level use (
     Ring_0 => 0,
     Ring_1 => 1,
     Ring_2 => 2,
     Ring_3 => 3
   );

   ----------------------------------------------------------------------------
   --  Range type for Descriptor tables.
   ----------------------------------------------------------------------------
   subtype Descriptor_Entry_Range is Natural;

   ----------------------------------------------------------------------------
   --  The format of the System Table Descriptor pointer used by the processor
   --  to load descriptor tables like the GDT and IDT.
   ----------------------------------------------------------------------------
   type System_Table_Descriptor is
      record
         Size   : Unsigned_16;
         Offset : System.Address;
      end record
   with Size => 48;
   for System_Table_Descriptor use
      record
         Size   at 0 range 0  .. 15;
         Offset at 0 range 16 .. 47;
      end record;
end System.x86.Descriptors;
