/**
 * Bindings for symbols and defines in `mach-o/stab.h`
 *
 * This file gives definitions supplementing <nlist.h> for permanent symbol
 * table entries of Mach-O files.  Modified from the BSD definitions.  The
 * modifications from the original definitions were changing what the values of
 * what was the n_other field (an unused field) which is now the n_sect field.
 * These modifications are required to support symbols in an arbitrary number of
 * sections not just the three sections (text, data and bss) in a BSD file.
 * The values of the defined constants have NOT been changed.
 *
 * These must have one of the N_STAB bits on.  The n_value fields are subject
 * to relocation according to the value of their n_sect field.  So for types
 * that refer to things in sections the n_sect field must be filled in with the
 * proper section ordinal.  For types that are not to have their n_value field
 * relocatated the n_sect field must be NO_SECT.
 *
 * This file was created based on the MacOSX 10.15 SDK.
 *
 * Copyright:
 * D Language Foundation 2020
 * Some documentation was extracted from the C headers
 * and is the property of Apple Inc.
 *
 * License: $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors: Mathias 'Geod24' Lang
 * Source: $(DRUNTIMESRC core/sys/darwin/mach/_stab.d)
 */
module core.sys.darwin.mach.stab;

extern(C):
nothrow:
@nogc:
pure:

/**
 * Symbolic debugger symbols.
 *
 * The comments give the conventional use for
 * ```
 *  .stabs "n_name", n_type, n_sect, n_desc, n_value
 * ```
 *
 * where n_type is the defined constant and not listed in the comment.  Other
 * fields not listed are zero. n_sect is the section ordinal the entry is
 * refering to.
 */
enum
{
    N_GSYM  = 0x20,    /// global symbol: name,,NO_SECT,type,0
    N_FNAME = 0x22,    /// procedure name (f77 kludge): name,,NO_SECT,0,0
    N_FUN   = 0x24,    /// procedure: name,,n_sect,linenumber,address
    N_STSYM = 0x26,    /// static symbol: name,,n_sect,type,address
    N_LCSYM = 0x28,    /// .lcomm symbol: name,,n_sect,type,address
    N_BNSYM = 0x2e,    /// begin nsect sym: 0,,n_sect,0,address
    N_AST   = 0x32,    /// AST file path: name,,NO_SECT,0,0
    N_OPT   = 0x3c,    /// emitted with gcc2_compiled and in gcc source
    N_RSYM  = 0x40,    /// register sym: name,,NO_SECT,type,register
    N_SLINE = 0x44,    /// src line: 0,,n_sect,linenumber,address
    N_ENSYM = 0x4e,    /// end nsect sym: 0,,n_sect,0,address
    N_SSYM  = 0x60,    /// structure elt: name,,NO_SECT,type,struct_offset
    N_SO    = 0x64,    /// source file name: name,,n_sect,0,address
    /**
     * Object file name: name,,(see below),0,st_mtime
     *
     * Historically N_OSO set n_sect to 0.
     * The N_OSO n_sect may instead hold the low byte of the cpusubtype value
     * from the Mach-O header.
     */
    N_OSO   = 0x66,
    N_LSYM  = 0x80,    /// local sym: name,,NO_SECT,type,offset
    N_BINCL = 0x82,    /// include file beginning: name,,NO_SECT,0,sum
    N_SOL   = 0x84,    /// #included file name: name,,n_sect,0,address
    N_PARAMS = 0x86,   /// compiler parameters: name,,NO_SECT,0,0
    N_VERSION = 0x88,  /// compiler version: name,,NO_SECT,0,0
    N_OLEVEL = 0x8A,   /// compiler -O level: name,,NO_SECT,0,0
    N_PSYM  = 0xa0,    /// parameter: name,,NO_SECT,type,offset
    N_EINCL = 0xa2,    /// include file end: name,,NO_SECT,0,0
    N_ENTRY = 0xa4,    /// alternate entry: name,,n_sect,linenumber,address
    N_LBRAC = 0xc0,    /// left bracket: 0,,NO_SECT,nesting level,address
    N_EXCL =  0xc2,    /// deleted include file: name,,NO_SECT,0,sum
    N_RBRAC = 0xe0,    /// right bracket: 0,,NO_SECT,nesting level,address
    N_BCOMM = 0xe2,    /// begin common: name,,NO_SECT,0,0
    N_ECOMM = 0xe4,    /// end common: name,,n_sect,0,0
    N_ECOML = 0xe8,    /// end common (local name): 0,,n_sect,0,address
    N_LENG =  0xfe,    /// second stab entry with length information

    // For the berkeley pascal compiler, pc(1):
    N_PC   = 0x30,    /// global pascal symbol: name,,NO_SECT,subtype,line
}
