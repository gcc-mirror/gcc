/**
 * Bindings for symbols and defines in `mach-o/nlist.h`
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
 * Source: $(DRUNTIMESRC core/sys/darwin/mach/_nlist.d)
 */
module core.sys.darwin.mach.nlist;

import core.stdc.config;

extern(C):
nothrow:
@nogc:
pure:

/**
 * An entry in a list of symbols for 64-bits architectures
 *
 * Said symbols can be used to describe many different type of data,
 * including STABS debug infos. Introduced in MacOSX 10.8 SDK.
 *
 * See_Also:
 * https://developer.apple.com/documentation/kernel/nlist_64
 */
struct nlist_64
{
    /// Compatibility alias, as `n_strx` is in an union in C code
    alias n_un = n_strx;

    /**
     * Index of this symbol's name into the string table
     *
     * All names are stored as NUL-terminated strings into the string table.
     * For historical reason, the very first entry into the string table is `0`,
     * hence all non-NULL names have an index > 0.
     */
    uint n_strx;

    /**
     * A bitfield that describes the type of this symbol
     *
     * In reality, this describes 4 fields:
     * - N_STAB (top 3 bits)
     * - N_PEXT (next 1 bit)
     * - N_TYPE (next 3 bits)
     * - N_EXT (last 1 bit)
     *
     * The enum values `N_STAB`, `N_PEXT`, `N_TYPE`, and `N_EXT` should be used
     * as masks to check which type this `nlist_64` actually is.
     */
    ubyte n_type;
    /// Section number (note that `0` means `NO_SECT`)
    ubyte n_sect;
    /* see <mach-o/stab.h> */
    ushort n_desc;
    /* value of this symbol (or stab offset) */
    ulong n_value;
    // Note: `n_value` *is* `uint64_t`, not `c_ulong` !
}

/// Mask to use with `nlist_64.n_type` to check what the entry describes
enum
{
    /**
     * If any of these bits set, a symbolic debugging entry
     *
     * Only symbolic debugging entries have some of the N_STAB bits set and if any
     * of these bits are set then it is a symbolic debugging entry (a stab).  In
     * which case then the values of the n_type field (the entire field) are given
     * in <mach-o/stab.h>
     */
    N_STAB = 0xe0,
    /// Private external symbol bit
    N_PEXT = 0x10,
    /// Mask for the type bits
    N_TYPE = 0x0e,  /* mask for the type bits */
    /// External symbol bit, set for external symbols
    N_EXT  = 0x01,
}

/// Values for `NTypeMask.N_TYPE` bits of the `nlist_64.n_type` field.
enum
{
    /// Undefined (`n_sect == NO_SECT`)
    N_UNDF = 0x0,
    /// Absolute (`n_sect == NO_SECT`)
    N_ABS  = 0x2,
    /// Defined in section number `nlist_64.n_sect`
    N_SECT = 0xe,
    /// Prebound undefined (defined in a dylib)
    N_PBUD = 0xc,
    /**
     * Indirect symbol
     *
     * If the type is `N_INDR` then the symbol is defined to be the same as
     * another symbol. In this case the `n_value` field is an index into
     * the string table of the other symbol's name. When the other symbol
     * is defined then they both take on the defined type and value.
     */
    N_INDR = 0xa,
}

/**
 * Symbol is not in any section
 *
 * If the type is N_SECT then the n_sect field contains an ordinal of the
 * section the symbol is defined in.  The sections are numbered from 1 and
 * refer to sections in order they appear in the load commands for the file
 * they are in.  This means the same ordinal may very well refer to different
 * sections in different files.
 *
 * The n_value field for all symbol table entries (including N_STAB's) gets
 * updated by the link editor based on the value of it's n_sect field and where
 * the section n_sect references gets relocated.  If the value of the n_sect
 * field is NO_SECT then it's n_value field is not changed by the link editor.
 */
enum NO_SECT = 0;

/// Maximum number of sections: 1 thru 255 inclusive
enum MAX_SECT = 255;

/**
 * Common symbols are represented by undefined (N_UNDF) external (N_EXT) types
 * who's values (n_value) are non-zero.  In which case the value of the n_value
 * field is the size (in bytes) of the common symbol.  The n_sect field is set
 * to NO_SECT.  The alignment of a common symbol may be set as a power of 2
 * between 2^1 and 2^15 as part of the n_desc field using the macros below. If
 * the alignment is not set (a value of zero) then natural alignment based on
 * the size is used.
 */
extern(D) ubyte GET_COMM_ALIGN(uint n_desc) @safe
{
    return (((n_desc) >> 8) & 0x0f);
}

/// Ditto
extern(D) ref ushort SET_COMM_ALIGN(return ref ushort n_desc, size_t wanted_align) @safe
{
    return n_desc = (((n_desc) & 0xf0ff) | (((wanted_align) & 0x0f) << 8));
}

/**
 * To support the lazy binding of undefined symbols in the dynamic link-editor,
 * the undefined symbols in the symbol table (the nlist structures) are marked
 * with the indication if the undefined reference is a lazy reference or
 * non-lazy reference.  If both a non-lazy reference and a lazy reference is
 * made to the same symbol the non-lazy reference takes precedence.  A reference
 * is lazy only when all references to that symbol are made through a symbol
 * pointer in a lazy symbol pointer section.
 *
 * The implementation of marking nlist structures in the symbol table for
 * undefined symbols will be to use some of the bits of the n_desc field as a
 * reference type.  The mask REFERENCE_TYPE will be applied to the n_desc field
 * of an nlist structure for an undefined symbol to determine the type of
 * undefined reference (lazy or non-lazy).
 *
 * The constants for the REFERENCE FLAGS are propagated to the reference table
 * in a shared library file.  In that case the constant for a defined symbol,
 * REFERENCE_FLAG_DEFINED, is also used.
 */
enum
{
    /// Reference type bits of the n_desc field of undefined symbols
    REFERENCE_TYPE = 0x7,

    /// types of references
    REFERENCE_FLAG_UNDEFINED_NON_LAZY = 0,
    /// Ditto
    REFERENCE_FLAG_UNDEFINED_LAZY     = 1,
    /// Ditto
    REFERENCE_FLAG_DEFINED            = 2,
    /// Ditto
    REFERENCE_FLAG_PRIVATE_DEFINED    = 3,
    /// Ditto
    REFERENCE_FLAG_PRIVATE_UNDEFINED_NON_LAZY = 4,
    /// Ditto
    REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY     = 5,

    /**
     * To simplify stripping of objects that use are used with the dynamic link
     * editor, the static link editor marks the symbols defined an object that are
     * referenced by a dynamicly bound object (dynamic shared libraries, bundles).
     * With this marking strip knows not to strip these symbols.
     */
    REFERENCED_DYNAMICALLY = 0x0010,
}

/**
 * For images created by the static link editor with the -twolevel_namespace
 * option in effect the flags field of the mach header is marked with
 * MH_TWOLEVEL.  And the binding of the undefined references of the image are
 * determined by the static link editor.  Which library an undefined symbol is
 * bound to is recorded by the static linker in the high 8 bits of the n_desc
 * field using the SET_LIBRARY_ORDINAL macro below.  The ordinal recorded
 * references the libraries listed in the Mach-O's LC_LOAD_DYLIB,
 * LC_LOAD_WEAK_DYLIB, LC_REEXPORT_DYLIB, LC_LOAD_UPWARD_DYLIB, and
 * LC_LAZY_LOAD_DYLIB, etc. load commands in the order they appear in the
 * headers.   The library ordinals start from 1.
 * For a dynamic library that is built as a two-level namespace image the
 * undefined references from module defined in another use the same nlist struct
 * an in that case SELF_LIBRARY_ORDINAL is used as the library ordinal.  For
 * defined symbols in all images they also must have the library ordinal set to
 * SELF_LIBRARY_ORDINAL.  The EXECUTABLE_ORDINAL refers to the executable
 * image for references from plugins that refer to the executable that loads
 * them.
 *
 * The DYNAMIC_LOOKUP_ORDINAL is for undefined symbols in a two-level namespace
 * image that are looked up by the dynamic linker with flat namespace semantics.
 * This ordinal was added as a feature in Mac OS X 10.3 by reducing the
 * value of MAX_LIBRARY_ORDINAL by one.  So it is legal for existing binaries
 * or binaries built with older tools to have 0xfe (254) dynamic libraries.  In
 * this case the ordinal value 0xfe (254) must be treated as a library ordinal
 * for compatibility.
 */
ubyte GET_LIBRARY_ORDINAL(uint n_desc) @safe { return ((n_desc) >> 8) & 0xff; }
/// Ditto
ref ushort SET_LIBRARY_ORDINAL(return scope ref ushort n_desc, uint ordinal) @safe
{
    return n_desc = (((n_desc) & 0x00ff) | (((ordinal) & 0xff) << 8));
}

/// Ditto
enum
{
    SELF_LIBRARY_ORDINAL   = 0x00,
    MAX_LIBRARY_ORDINAL    = 0xfd,
    DYNAMIC_LOOKUP_ORDINAL = 0xfe,
    EXECUTABLE_ORDINAL     = 0xff,
}

/**
 * The bit 0x0020 of the n_desc field is used for two non-overlapping purposes
 * and has two different symbolic names, N_NO_DEAD_STRIP and N_DESC_DISCARDED.
 */
enum
{
    /**
     * Symbol is not to be dead stripped
     *
     * The N_NO_DEAD_STRIP bit of the n_desc field only ever appears in a
     * relocatable .o file (MH_OBJECT filetype). And is used to indicate to the
     * static link editor it is never to dead strip the symbol.
     */
    N_NO_DEAD_STRIP = 0x0020,

    /**
     * Symbol is discarded
     *
     * The N_DESC_DISCARDED bit of the n_desc field never appears in linked image.
     * But is used in very rare cases by the dynamic link editor to mark an in
     * memory symbol as discared and longer used for linking.
     */
    N_DESC_DISCARDED =0x0020,

    /**
     * Symbol is weak referenced
     *
     * The N_WEAK_REF bit of the n_desc field indicates to the dynamic linker that
     * the undefined symbol is allowed to be missing and is to have the address of
     * zero when missing.
     */
    N_WEAK_REF = 0x0040,

    /**
     * Coalesed symbol is a weak definition
     *
     * The N_WEAK_DEF bit of the n_desc field indicates to the static and dynamic
     * linkers that the symbol definition is weak, allowing a non-weak symbol to
     * also be used which causes the weak definition to be discared.  Currently this
     * is only supported for symbols in coalesed sections.
     */
    N_WEAK_DEF = 0x0080,

    /**
     * Reference to a weak symbol
     *
     * The N_REF_TO_WEAK bit of the n_desc field indicates to the dynamic linker
     * that the undefined symbol should be resolved using flat namespace searching.
     */
    N_REF_TO_WEAK = 0x0080,

    /**
     * Symbol is a Thumb function (ARM)
     *
     * The N_ARM_THUMB_DEF bit of the n_desc field indicates that the symbol is
     * a defintion of a Thumb function.
     */
    N_ARM_THUMB_DEF = 0x0008,

    /**
     * The N_SYMBOL_RESOLVER bit of the n_desc field indicates that the
     * that the function is actually a resolver function and should
     * be called to get the address of the real function to use.
     * This bit is only available in .o files (MH_OBJECT filetype)
     */
    N_SYMBOL_RESOLVER =  0x0100,

    /**
     * The N_ALT_ENTRY bit of the n_desc field indicates that the
     * symbol is pinned to the previous content.
     */
    N_ALT_ENTRY = 0x0200,

    /**
     * The N_COLD_FUNC bit of the n_desc field indicates that the symbol is used
     * infrequently and the linker should order it towards the end of the section.
     */
    N_COLD_FUNC = 0x0400,
}
