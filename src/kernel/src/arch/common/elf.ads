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
--  ELF
--
--  Purpose:
--    This package provides definitions and functionality for working with
--    the ELF executable file format.
-------------------------------------------------------------------------------
package Elf is
   pragma Pure (Elf);
   pragma Preelaborate (Elf);

   use Interfaces;

   ----------------------------------------------------------------------------
   --  ELF file magic number type.
   ----------------------------------------------------------------------------
   type Elf_File_Magic_Number is new String (1 .. 4);

   ----------------------------------------------------------------------------
   --  ELF File class
   --  Determines the kind of executable this file represents.
   ----------------------------------------------------------------------------
   type EI_CLASS is (
     ELFCLASSNONE,
     ELFCLASS32,
     ELFCLASS64
   );
   for EI_CLASS'Size use 8;
   for EI_CLASS use (
     ELFCLASSNONE => 0,
     ELFCLASS32   => 1,
     ELFCLASS64   => 2
   );

   ----------------------------------------------------------------------------
   --  ELF Data Encoding type.
   --  Determines the endianness of the executable.
   ----------------------------------------------------------------------------
   type EI_DATA is (
     ELFDATANONE,
     ELFDATA2LSB,
     ELFDATA2MSB
   );
   for EI_DATA'Size use 8;
   for EI_DATA use (
     ELFDATANONE => 0,
     ELFDATA2LSB => 1,
     ELFDATA2MSB => 2
   );

   ----------------------------------------------------------------------------
   --  ELF File Version.
   --  Denotes the version of the ELF file encoding.
   ----------------------------------------------------------------------------
   type EI_VERSION is (
     EV_NONE,
     EV_CURRENT,
     EV_NUM
   );
   for EI_VERSION'Size use 8;
   for EI_VERSION use (
     EV_NONE    => 0,
     EV_CURRENT => 1,
     EV_NUM     => 2
   );

   ----------------------------------------------------------------------------
   --  ELF File ABI.
   --  Determines the ABI of the executable.
   ----------------------------------------------------------------------------
   type EI_OSABI is (
     ELFOSABI_SYSV,
     ELFOSABI_HPUX,
     ELFOSABI_NETBSD,
     ELFOSABI_GNU,
     ELFOSABI_SOLARIS,
     ELFOSABI_AIX,
     ELFOSABI_IRIX,
     ELFOSABI_FREEBSD,
     ELFOSABI_TRU64,
     ELFOSABI_MODESTO,
     ELFOSABI_OPENBSD,
     ELFOSABI_ARM_AEABI,
     ELFOSABI_ARM,
     ELFOSABI_STANDALONE
   );
   for EI_OSABI'Size use 8;
   for EI_OSABI use (
     ELFOSABI_SYSV       => 0,
     ELFOSABI_HPUX       => 1,
     ELFOSABI_NETBSD     => 2,
     ELFOSABI_GNU        => 3,
     ELFOSABI_SOLARIS    => 6,
     ELFOSABI_AIX        => 7,
     ELFOSABI_IRIX       => 8,
     ELFOSABI_FREEBSD    => 9,
     ELFOSABI_TRU64      => 10,
     ELFOSABI_MODESTO    => 11,
     ELFOSABI_OPENBSD    => 12,
     ELFOSABI_ARM_AEABI  => 64,
     ELFOSABI_ARM        => 97,
     ELFOSABI_STANDALONE => 255
   );

   ----------------------------------------------------------------------------
   --  ELF File ABI Version.
   --  The version of the ABI.
   ----------------------------------------------------------------------------
   type EI_ABIVERSION is (
      Default
   );
   for EI_ABIVERSION'Size use 8;
   for EI_ABIVERSION use (
      Default => 0
   );

   ----------------------------------------------------------------------------
   --  ELF File Machine type.
   --  This denotes the machine architecture the executable is targeting.
   ----------------------------------------------------------------------------
   type Elf_File_Machine_Type is (
      EM_NONE,
      EM_M32,
      EM_SPARC,
      EM_386,
      EM_68K,
      EM_88K,
      EM_860,
      EM_MIPS,
      EM_MIPS_RS3_LE,
      EM_PARISC,
      EM_SPARC32PLUS,
      EM_PPC,
      EM_PPC64,
      EM_S390,
      EM_ARM,
      EM_SH,
      EM_SPARCV9,
      EM_IA_64,
      EM_X86_64,
      EM_VAX
   );
   for Elf_File_Machine_Type'Size use 16;
   for Elf_File_Machine_Type use (
      EM_NONE         => 0,
      EM_M32          => 1,
      EM_SPARC        => 2,
      EM_386          => 3,
      EM_68K          => 4,
      EM_88K          => 5,
      EM_860          => 7,
      EM_MIPS         => 8,
      EM_MIPS_RS3_LE  => 10,
      EM_PARISC       => 15,
      EM_SPARC32PLUS  => 18,
      EM_PPC          => 20,
      EM_PPC64        => 21,
      EM_S390         => 22,
      EM_ARM          => 40,
      EM_SH           => 42,
      EM_SPARCV9      => 43,
      EM_IA_64        => 50,
      EM_X86_64       => 62,
      EM_VAX          => 75
   );

   ----------------------------------------------------------------------------
   --  ELF file identifier type.
   --  Contains variables that determines the executable type and target.
   ----------------------------------------------------------------------------
   type Elf_Identifier is
      record
         Magic_Number     : Elf_File_Magic_Number;
         File_Class       : EI_CLASS;
         File_Encoding    : EI_DATA;
         File_Version     : EI_VERSION;
         File_ABI         : EI_OSABI;
         File_ABI_Version : EI_ABIVERSION;
         Padding          : String (1 .. 7);
      end record;
   for Elf_Identifier'Size use 128;
   for Elf_Identifier use
      record
         Magic_Number     at 0 range 0 .. 31;
         File_Class       at 4 range 0 .. 7;
         File_Encoding    at 5 range 0 .. 7;
         File_Version     at 6 range 0 .. 7;
         File_ABI         at 7 range 0 .. 7;
         File_ABI_Version at 8 range 0 .. 7;
         Padding          at 9 range 0 .. 55;
      end record;

   ----------------------------------------------------------------------------
   --  ELF File Type.
   --  This denotes the type of ELF file. Whether it is an executable or a
   --  relocatable binary.
   ----------------------------------------------------------------------------
   type Elf_File_Type is (
     ET_NONE,
     ET_REL,
     ET_EXEC,
     ET_DYN,
     ET_CORE,
     ET_NUM,
     ET_LOOS,
     ET_HIOS,
     ET_LOPROC,
     ET_HIPROC
   );
   for Elf_File_Type'Size use 16;
   for Elf_File_Type use (
     ET_NONE   => 0,
     ET_REL    => 1,
     ET_EXEC   => 2,
     ET_DYN    => 3,
     ET_CORE   => 4,
     ET_NUM    => 5,
     ET_LOOS   => 16#FE00#,
     ET_HIOS   => 16#FEFF#,
     ET_LOPROC => 16#FF00#,
     ET_HIPROC => 16#FFFF#
   );

   ----------------------------------------------------------------------------
   --  ELF File Version.
   --  This type denotes the encoding version of the ELF binary.
   ----------------------------------------------------------------------------
   type Elf_File_Version is (
     EV_NONE,
     EV_CURRENT
   );
   for Elf_File_Version'Size use 32;
   for Elf_File_Version use (
     EV_NONE => 0,
     EV_CURRENT => 1
   );

   ----------------------------------------------------------------------------
   --  ELF Section Index Type.
   ----------------------------------------------------------------------------
   subtype Elf_Section_Index is Unsigned_16;

   ----------------------------------------------------------------------------
   --  ELF32 File Offset Type.
   --  Type for the offset into a 32bit ELF binary.
   ----------------------------------------------------------------------------
   subtype Elf32_File_Offset is Unsigned_32;

   ----------------------------------------------------------------------------
   --  ELF32 Address type.
   ----------------------------------------------------------------------------
   subtype Elf32_Address is Unsigned_32;

   ----------------------------------------------------------------------------
   --  ELF64 File Offset Type.
   --  Type for the offset into a 64bit ELF binary.
   ----------------------------------------------------------------------------
   subtype Elf64_File_Offset is Unsigned_64;

   ----------------------------------------------------------------------------
   --  ELF64 Address type.
   ----------------------------------------------------------------------------
   subtype Elf64_Address is Unsigned_64;

   ----------------------------------------------------------------------------
   --  ELF32 File Header type.
   ----------------------------------------------------------------------------
   type Elf32_File_Header is
      record
         e_ident     : Elf_Identifier;
         e_type      : Elf_File_Type;
         e_machine   : Elf_File_Machine_Type;
         e_version   : Elf_File_Version;
         e_entry     : Elf32_Address;
         e_phoff     : Elf32_File_Offset;
         e_shoff     : Elf32_File_Offset;
         e_flags     : Unsigned_32;
         e_ehsize    : Unsigned_16;
         e_phentsize : Unsigned_16;
         e_phnum     : Unsigned_16;
         e_shentsize : Unsigned_16;
         e_shnum     : Unsigned_16;
         e_shstrndx  : Unsigned_16;
      end record;
   for Elf32_File_Header'Size use 416;
   for Elf32_File_Header use
      record
         e_ident     at 0  range 0 .. 127;
         e_type      at 16 range 0 .. 15;
         e_machine   at 18 range 0 .. 15;
         e_version   at 20 range 0 .. 31;
         e_entry     at 24 range 0 .. 31;
         e_phoff     at 28 range 0 .. 31;
         e_shoff     at 32 range 0 .. 31;
         e_flags     at 36 range 0 .. 31;
         e_ehsize    at 40 range 0 .. 15;
         e_phentsize at 42 range 0 .. 15;
         e_phnum     at 44 range 0 .. 15;
         e_shentsize at 46 range 0 .. 15;
         e_shnum     at 48 range 0 .. 15;
         e_shstrndx  at 50 range 0 .. 15;
      end record;

   ----------------------------------------------------------------------------
   --  ELF Section Header Type.
   --  Determines the type of this ELF file section.
   ----------------------------------------------------------------------------
   type Elf_Section_Header_Type is (
     SHT_NULL,
     SHT_PROGBITS,
     SHT_SYMTAB,
     SHT_STRTAB,
     SHT_RELA,
     SHT_HASH,
     SHT_DYNAMIC,
     SHT_NOTE,
     SHT_NOBITS,
     SHT_REL,
     SHT_SHLIB,
     SHT_DYNSYM,
     SHT_LOPROC,
     SHT_HIPROC,
     SHT_LOUSER,
     SHT_HIUSER
   );
   for Elf_Section_Header_Type use (
     SHT_NULL     => 0,
     SHT_PROGBITS => 1,
     SHT_SYMTAB   => 2,
     SHT_STRTAB   => 3,
     SHT_RELA     => 4,
     SHT_HASH     => 5,
     SHT_DYNAMIC  => 6,
     SHT_NOTE     => 7,
     SHT_NOBITS   => 8,
     SHT_REL      => 9,
     SHT_SHLIB    => 10,
     SHT_DYNSYM   => 11,
     SHT_LOPROC   => 16#70000000#,
     SHT_HIPROC   => 16#7FFFFFFF#,
     SHT_LOUSER   => 16#80000000#,
     SHT_HIUSER   => 16#8FFFFFFF#
   );

   ----------------------------------------------------------------------------
   --  ELF32 Section Flags Type.
   --  Determines the flags for this ELF section.
   ----------------------------------------------------------------------------
   type Elf32_Section_Flags is
      record
         SHF_WRITE          : Boolean;
         SHF_ALLOC          : Boolean;
         SHF_EXECINSTR      : Boolean;
         SHF_RELA_LIVEPATCH : Boolean;
         SHF_RO_AFTER_INIT  : Boolean;
         SHF_MASKPROC       : Boolean;
      end record;
   for Elf32_Section_Flags'Size use 32;
   for Elf32_Section_Flags use
      record
         SHF_WRITE          at 0  range 0 .. 0;
         SHF_ALLOC          at 0  range 1 .. 1;
         SHF_EXECINSTR      at 0  range 2 .. 2;
         SHF_RELA_LIVEPATCH at 0  range 28 .. 28;
         SHF_RO_AFTER_INIT  at 0  range 29 .. 29;
         SHF_MASKPROC       at 0  range 31 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  ELF String Table Offset Type.
   ----------------------------------------------------------------------------
   subtype Elf_String_Table_Offset is Unsigned_32;

   ----------------------------------------------------------------------------
   --  ELF Section Header Index Type.
   ----------------------------------------------------------------------------
   subtype Elf_Section_Header_Index is Unsigned_32;

   ----------------------------------------------------------------------------
   --  ELF32 Section Header.
   ----------------------------------------------------------------------------
   type Elf32_Section_Header is
      record
         sh_name      : Elf_String_Table_Offset;
         sh_type      : Elf_Section_Header_Type;
         sh_flags     : Unsigned_32;
         sh_addr      : Elf32_Address;
         sh_offset    : Elf32_File_Offset;
         sh_size      : Unsigned_32;
         sh_link      : Elf_Section_Header_Index;
         sh_info      : Unsigned_32;
         sh_addralign : Unsigned_32;
         sh_entsize   : Unsigned_32;
      end record;
   for Elf32_Section_Header'Size use 320;
   for Elf32_Section_Header use
      record
         sh_name      at 0  range 0 .. 31;
         sh_type      at 4  range 0 .. 31;
         sh_flags     at 8  range 0 .. 31;
         sh_addr      at 12 range 0 .. 31;
         sh_offset    at 16 range 0 .. 31;
         sh_size      at 20 range 0 .. 31;
         sh_link      at 24 range 0 .. 31;
         sh_info      at 28 range 0 .. 31;
         sh_addralign at 32 range 0 .. 31;
         sh_entsize   at 36 range 0 .. 31;
      end record;

   ----------------------------------------------------------------------------
   --  64bit ELF File header.
   ----------------------------------------------------------------------------
   type Elf64_File_Header is
      record
         e_ident     : Elf_Identifier;
         e_type      : Elf_File_Type;
         e_machine   : Elf_File_Machine_Type;
         e_version   : Elf_File_Version;
         e_entry     : Elf64_Address;
         e_phoff     : Elf64_File_Offset;
         e_shoff     : Elf64_File_Offset;
         e_flags     : Unsigned_32;
         e_ehsize    : Unsigned_16;
         e_phentsize : Unsigned_16;
         e_phnum     : Unsigned_16;
         e_shentsize : Unsigned_16;
         e_shnum     : Unsigned_16;
         e_shstrndx  : Unsigned_16;
      end record;
   for Elf64_File_Header'Size use 512;
   for Elf64_File_Header use
      record
         e_ident     at 0  range 0 .. 127;
         e_type      at 16 range 0 .. 15;
         e_machine   at 18 range 0 .. 15;
         e_version   at 20 range 0 .. 31;
         e_entry     at 24 range 0 .. 63;
         e_phoff     at 32 range 0 .. 63;
         e_shoff     at 40 range 0 .. 63;
         e_flags     at 48 range 0 .. 31;
         e_ehsize    at 52 range 0 .. 15;
         e_phentsize at 54 range 0 .. 15;
         e_phnum     at 56 range 0 .. 15;
         e_shentsize at 58 range 0 .. 15;
         e_shnum     at 60 range 0 .. 15;
         e_shstrndx  at 62 range 0 .. 15;
      end record;

   ----------------------------------------------------------------------------
   --  Validate_Elf_Header_Magic_Number
   --
   --  Purpose:
   --    Validates the magic number contained in the ELF file header.
   --    This function determines whether this is a valid ELF file.
   --
   --  Exceptions:
   --    None.
   ----------------------------------------------------------------------------
   function Validate_Elf_Header_Magic_Number (
     Magic_Number : Elf_File_Magic_Number
   ) return Boolean;
end Elf;
