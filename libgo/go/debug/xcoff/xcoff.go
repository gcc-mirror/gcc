// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package xcoff

// File Header.
type FileHeader32 struct {
	Fmagic   uint16 // Target machine
	Fnscns   uint16 // Number of sections
	Ftimedat int32  // Time and date of file creation
	Fsymptr  uint32 // Byte offset to symbol table start
	Fnsyms   int32  // Number of entries in symbol table
	Fopthdr  uint16 // Number of bytes in optional header
	Fflags   uint16 // Flags
}

type FileHeader64 struct {
	Fmagic   uint16 // Target machine
	Fnscns   uint16 // Number of sections
	Ftimedat int32  // Time and date of file creation
	Fsymptr  uint64 // Byte offset to symbol table start
	Fopthdr  uint16 // Number of bytes in optional header
	Fflags   uint16 // Flags
	Fnsyms   int32  // Number of entries in symbol table
}

const (
	FILHSZ_32 = 20
	FILHSZ_64 = 24
)
const (
	U802TOCMAGIC = 0737 // AIX 32-bit XCOFF
	U64_TOCMAGIC = 0767 // AIX 64-bit XCOFF
)

// Flags that describe the type of the object file.
const (
	F_RELFLG    = 0x0001
	F_EXEC      = 0x0002
	F_LNNO      = 0x0004
	F_FDPR_PROF = 0x0010
	F_FDPR_OPTI = 0x0020
	F_DSA       = 0x0040
	F_VARPG     = 0x0100
	F_DYNLOAD   = 0x1000
	F_SHROBJ    = 0x2000
	F_LOADONLY  = 0x4000
)

// Section Header.
type SectionHeader32 struct {
	Sname    [8]byte // Section name
	Spaddr   uint32  // Physical address
	Svaddr   uint32  // Virtual address
	Ssize    uint32  // Section size
	Sscnptr  uint32  // Offset in file to raw data for section
	Srelptr  uint32  // Offset in file to relocation entries for section
	Slnnoptr uint32  // Offset in file to line number entries for section
	Snreloc  uint16  // Number of relocation entries
	Snlnno   uint16  // Number of line number entries
	Sflags   uint32  // Flags to define the section type
}

type SectionHeader64 struct {
	Sname    [8]byte // Section name
	Spaddr   uint64  // Physical address
	Svaddr   uint64  // Virtual address
	Ssize    uint64  // Section size
	Sscnptr  uint64  // Offset in file to raw data for section
	Srelptr  uint64  // Offset in file to relocation entries for section
	Slnnoptr uint64  // Offset in file to line number entries for section
	Snreloc  uint32  // Number of relocation entries
	Snlnno   uint32  // Number of line number entries
	Sflags   uint32  // Flags to define the section type
	Spad     uint32  // Needs to be 72 bytes long
}

// Flags defining the section type.
const (
	STYP_DWARF  = 0x0010
	STYP_TEXT   = 0x0020
	STYP_DATA   = 0x0040
	STYP_BSS    = 0x0080
	STYP_EXCEPT = 0x0100
	STYP_INFO   = 0x0200
	STYP_TDATA  = 0x0400
	STYP_TBSS   = 0x0800
	STYP_LOADER = 0x1000
	STYP_DEBUG  = 0x2000
	STYP_TYPCHK = 0x4000
	STYP_OVRFLO = 0x8000
)
const (
	SSUBTYP_DWINFO  = 0x10000 // DWARF info section
	SSUBTYP_DWLINE  = 0x20000 // DWARF line-number section
	SSUBTYP_DWPBNMS = 0x30000 // DWARF public names section
	SSUBTYP_DWPBTYP = 0x40000 // DWARF public types section
	SSUBTYP_DWARNGE = 0x50000 // DWARF aranges section
	SSUBTYP_DWABREV = 0x60000 // DWARF abbreviation section
	SSUBTYP_DWSTR   = 0x70000 // DWARF strings section
	SSUBTYP_DWRNGES = 0x80000 // DWARF ranges section
	SSUBTYP_DWLOC   = 0x90000 // DWARF location lists section
	SSUBTYP_DWFRAME = 0xA0000 // DWARF frames section
	SSUBTYP_DWMAC   = 0xB0000 // DWARF macros section
)

// Symbol Table Entry.
type SymEnt32 struct {
	Nname   [8]byte // Symbol name
	Nvalue  uint32  // Symbol value
	Nscnum  int16   // Section number of symbol
	Ntype   uint16  // Basic and derived type specification
	Nsclass int8    // Storage class of symbol
	Nnumaux int8    // Number of auxiliary entries
}

type SymEnt64 struct {
	Nvalue  uint64 // Symbol value
	Noffset uint32 // Offset of the name in string table or .debug section
	Nscnum  int16  // Section number of symbol
	Ntype   uint16 // Basic and derived type specification
	Nsclass int8   // Storage class of symbol
	Nnumaux int8   // Number of auxiliary entries
}

const SYMESZ = 18

// Storage Class.
const (
	C_NULL    = 0   // Symbol table entry marked for deletion
	C_EXT     = 2   // External symbol
	C_STAT    = 3   // Static symbol
	C_BLOCK   = 100 // Beginning or end of inner block
	C_FCN     = 101 // Beginning or end of function
	C_FILE    = 103 // Source file name and compiler information
	C_HIDEXT  = 107 // Unnamed external symbol
	C_BINCL   = 108 // Beginning of include file
	C_EINCL   = 109 // End of include file
	C_WEAKEXT = 111 // Weak external symbol
	C_DWARF   = 112 // DWARF symbol
	C_GSYM    = 128 // Global variable
	C_LSYM    = 129 // Automatic variable allocated on stack
	C_PSYM    = 130 // Argument to subroutine allocated on stack
	C_RSYM    = 131 // Register variable
	C_RPSYM   = 132 // Argument to function or procedure stored in register
	C_STSYM   = 133 // Statically allocated symbol
	C_BCOMM   = 135 // Beginning of common block
	C_ECOML   = 136 // Local member of common block
	C_ECOMM   = 137 // End of common block
	C_DECL    = 140 // Declaration of object
	C_ENTRY   = 141 // Alternate entry
	C_FUN     = 142 // Function or procedure
	C_BSTAT   = 143 // Beginning of static block
	C_ESTAT   = 144 // End of static block
	C_GTLS    = 145 // Global thread-local variable
	C_STTLS   = 146 // Static thread-local variable
)

// csect Auxiliary Entry.
type AuxCSect32 struct {
	Xscnlen   int32  // Length or symbol table index
	Xparmhash uint32 // Offset of parameter type-check string
	Xsnhash   uint16 // .typchk section number
	Xsmtyp    uint8  // Symbol alignment and type
	Xsmclas   uint8  // Storage-mapping class
	Xstab     uint32 // Reserved
	Xsnstab   uint16 // Reserved
}

type AuxCSect64 struct {
	Xscnlenlo uint32 // Lower 4 bytes of length or symbol table index
	Xparmhash uint32 // Offset of parameter type-check string
	Xsnhash   uint16 // .typchk section number
	Xsmtyp    uint8  // Symbol alignment and type
	Xsmclas   uint8  // Storage-mapping class
	Xscnlenhi int32  // Upper 4 bytes of length or symbol table index
	Xpad      uint8  // Unused
	Xauxtype  uint8  // Type of auxiliary entry
}

// Symbol type field.
const (
	XTY_ER = 0 // External reference
	XTY_SD = 1 // Section definition
	XTY_LD = 2 // Label definition
	XTY_CM = 3 // Common csect definition
)

// Storage-mapping class.
const (
	XMC_PR     = 0  // Program code
	XMC_RO     = 1  // Read-only constant
	XMC_DB     = 2  // Debug dictionary table
	XMC_TC     = 3  // TOC entry
	XMC_UA     = 4  // Unclassified
	XMC_RW     = 5  // Read/Write data
	XMC_GL     = 6  // Global linkage
	XMC_XO     = 7  // Extended operation
	XMC_SV     = 8  // 32-bit supervisor call descriptor
	XMC_BS     = 9  // BSS class
	XMC_DS     = 10 // Function descriptor
	XMC_UC     = 11 // Unnamed FORTRAN common
	XMC_TC0    = 15 // TOC anchor
	XMC_TD     = 16 // Scalar data entry in the TOC
	XMC_SV64   = 17 // 64-bit supervisor call descriptor
	XMC_SV3264 = 18 // Supervisor call descriptor for both 32-bit and 64-bit
	XMC_TL     = 20 // Read/Write thread-local data
	XMC_UL     = 21 // Read/Write thread-local data (.tbss)
	XMC_TE     = 22 // TOC entry
)

// Loader Header.
type LoaderHeader32 struct {
	Lversion int32  // Loader section version number
	Lnsyms   int32  // Number of symbol table entries
	Lnreloc  int32  // Number of relocation table entries
	Listlen  uint32 // Length of import file ID string table
	Lnimpid  int32  // Number of import file IDs
	Limpoff  uint32 // Offset to start of import file IDs
	Lstlen   uint32 // Length of string table
	Lstoff   uint32 // Offset to start of string table
}

type LoaderHeader64 struct {
	Lversion int32  // Loader section version number
	Lnsyms   int32  // Number of symbol table entries
	Lnreloc  int32  // Number of relocation table entries
	Listlen  uint32 // Length of import file ID string table
	Lnimpid  int32  // Number of import file IDs
	Lstlen   uint32 // Length of string table
	Limpoff  uint64 // Offset to start of import file IDs
	Lstoff   uint64 // Offset to start of string table
	Lsymoff  uint64 // Offset to start of symbol table
	Lrldoff  uint64 // Offset to start of relocation entries
}

const (
	LDHDRSZ_32 = 32
	LDHDRSZ_64 = 56
)

// Loader Symbol.
type LoaderSymbol32 struct {
	Lname   [8]byte // Symbol name or byte offset into string table
	Lvalue  uint32  // Address field
	Lscnum  int16   // Section number containing symbol
	Lsmtype int8    // Symbol type, export, import flags
	Lsmclas int8    // Symbol storage class
	Lifile  int32   // Import file ID; ordinal of import file IDs
	Lparm   uint32  // Parameter type-check field
}

type LoaderSymbol64 struct {
	Lvalue  uint64 // Address field
	Loffset uint32 // Byte offset into string table of symbol name
	Lscnum  int16  // Section number containing symbol
	Lsmtype int8   // Symbol type, export, import flags
	Lsmclas int8   // Symbol storage class
	Lifile  int32  // Import file ID; ordinal of import file IDs
	Lparm   uint32 // Parameter type-check field
}
