package x86.Descriptors is
   pragma Preelaborate (x86.Descriptors);

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

   subtype Descriptor_Entry_Range is Natural;
end x86.Descriptors;
