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
--  SYSTEM.X86.TSS
--
--  Purpose:
--    This package contains definitions for the x86 Task State Segment.
-------------------------------------------------------------------------------
package x86.TSS is
   pragma Preelaborate (x86.TSS);

   use Interfaces;

   ----------------------------------------------------------------------------
   --  Task State Segment Type.
   --  Refer to Page 242. Intel IA-32 SDM 3a.
   ----------------------------------------------------------------------------
   type Task_State_Segment is
      record
         LINK        : Unsigned_16;
         Reserved    : Unsigned_16 := 0;
         ESP0        : Unsigned_32;
         SS0         : Unsigned_16;
         Reserved_1  : Unsigned_16 := 0;
         ESP1        : Unsigned_32;
         SS1         : Unsigned_16;
         Reserved_2  : Unsigned_16 := 0;
         ESP2        : Unsigned_32;
         SS2         : Unsigned_16;
         Reserved_3  : Unsigned_16 := 0;
         CR3         : Unsigned_32;
         EIP         : Unsigned_32;
         EFLAGS      : Unsigned_32;
         EAX         : Unsigned_32;
         ECX         : Unsigned_32;
         EDX         : Unsigned_32;
         EBX         : Unsigned_32;
         ESP         : Unsigned_32;
         EBP         : Unsigned_32;
         ESI         : Unsigned_32;
         EDI         : Unsigned_32;
         ES          : Unsigned_16;
         Reserved_4  : Unsigned_16 := 0;
         CS          : Unsigned_16;
         Reserved_5  : Unsigned_16 := 0;
         SS          : Unsigned_16;
         Reserved_6  : Unsigned_16 := 0;
         DS          : Unsigned_16;
         Reserved_7  : Unsigned_16 := 0;
         FS          : Unsigned_16;
         Reserved_8  : Unsigned_16 := 0;
         GS          : Unsigned_16;
         Reserved_9  : Unsigned_16 := 0;
         LDTR        : Unsigned_16;
         Reserved_10 : Unsigned_16 := 0;
         Reserved_11 : Unsigned_16 := 0;
         IOPB        : Unsigned_16;
      end record
   with Size => 832;
   for Task_State_Segment use
      record
         LINK        at 0   range 0  .. 15;
         Reserved    at 0   range 16 .. 31;
         ESP0        at 4   range 0  .. 31;
         SS0         at 8   range 0  .. 15;
         Reserved_1  at 8   range 16 .. 31;
         ESP1        at 12  range 0  .. 31;
         SS1         at 16  range 0  .. 15;
         Reserved_2  at 16  range 16 .. 31;
         ESP2        at 20  range 0  .. 31;
         SS2         at 24  range 0  .. 15;
         Reserved_3  at 24  range 16 .. 31;
         CR3         at 28  range 0  .. 31;
         EIP         at 32  range 0  .. 31;
         EFLAGS      at 36  range 0  .. 31;
         EAX         at 40  range 0  .. 31;
         ECX         at 44  range 0  .. 31;
         EDX         at 48  range 0  .. 31;
         EBX         at 52  range 0  .. 31;
         ESP         at 56  range 0  .. 31;
         EBP         at 60  range 0  .. 31;
         ESI         at 64  range 0  .. 31;
         EDI         at 68  range 0  .. 31;
         ES          at 72  range 0  .. 15;
         Reserved_4  at 72  range 16 .. 31;
         CS          at 76  range 0  .. 15;
         Reserved_5  at 76  range 16 .. 31;
         SS          at 80  range 0  .. 15;
         Reserved_6  at 80  range 16 .. 31;
         DS          at 84  range 0  .. 15;
         Reserved_7  at 84  range 16 .. 31;
         FS          at 88  range 0  .. 15;
         Reserved_8  at 88  range 16 .. 31;
         GS          at 92  range 0  .. 15;
         Reserved_9  at 92  range 16 .. 31;
         LDTR        at 96  range 0  .. 15;
         Reserved_10 at 96  range 16 .. 31;
         Reserved_11 at 100 range 0  .. 15;
         IOPB        at 100 range 16 .. 31;
      end record;
end x86.TSS;
