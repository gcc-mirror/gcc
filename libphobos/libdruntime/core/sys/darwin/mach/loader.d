/**
 * This file describes the format of Mach-O object files.
 *
 * D header file for `mach-o/loader.h` from the macOS 10.15 SDK.
 *
 * Copyright: Copyright Digital Mars 2010-2019.
 * License: $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors: Jacob Carlborg
 * Version: Initial created: Feb 20, 2010-2018
 * Source: $(DRUNTIMESRC core/sys/darwin/mach/_loader.d)
 */
module core.sys.darwin.mach.loader;

import core.stdc.config;

version (CoreDdoc)
{
    /**
     * The 32-bit mach header appears at the very beginning of the object file
     * for 32-bit architectures.
     */
    struct mach_header
    {
        /// Mach magic number identifier.
        uint magic;

        /// Cpu specifier.
        int cputype;

        /// Machine specifier.
        int cpusubtype;

        /// Type of file.
        uint filetype;

        /// Number of load commands.
        uint ncmds;

        /// The size of all the load commands.
        uint sizeofcmds;

        /// Flags.
        uint flags;
    }

    /// Constant for the magic field of the mach_header (32-bit architectures)
    enum
    {
        /// The mach magic number
        MH_MAGIC,

        /// NXSwapInt(MH_MAGIC)
        MH_CIGAM
    }

    /**
     * The 64-bit mach header appears at the very beginning of object files for
     * 64-bit architectures.
     */
    struct mach_header_64
    {
        /// Mach magic number identifier.
        uint magic;

        /// Cpu specifier.
        int cputype;

        /// Machine specifier.
        int cpusubtype;

        /// Type of file.
        uint filetype;

        /// Number of load commands.
        uint ncmds;

        /// The size of all the load commands.
        uint sizeofcmds;

        /// Flags.
        uint flags;

        /// Reserved.
        uint reserved;
    }

    /// Constant for the magic field of the mach_header_64 (64-bit architectures)
    enum
    {
        /// The 64-bit mach magic number.
        MH_MAGIC_64,

        /// NXSwapInt(MH_MAGIC_64).
        MH_CIGAM_64
    }

    /**
     * The layout of the file depends on the filetype. For all but the MH_OBJECT
     * file type the segments are padded out and aligned on a segment alignment
     * boundary for efficient demand pageing. The MH_EXECUTE, MH_FVMLIB,
     * MH_DYLIB, MH_DYLINKER and MH_BUNDLE file types also have the headers
     * included as part of their first segment.
     *
     * The file type MH_OBJECT is a compact format intended as output of the
     * assembler and input (and possibly output) of the link editor (the .o
     * format). All sections are in one unnamed segment with no segment padding.
     * This format is used as an executable format when the file is so small the
     * segment padding greatly increases its size.
     *
     * The file type MH_PRELOAD is an executable format intended for things that
     * are not executed under the kernel (proms, stand alones, kernels, etc).
     * The format can be executed under the kernel but may demand paged it and
     * not preload it before execution.
     *
     * A core file is in MH_CORE format and can be any in an arbitrary legal
     * Mach-O file.
     *
     * Constants for the filetype field of the mach_header
     */
    enum
    {
        /// Relocatable object file.
        MH_OBJECT,

        /// Demand paged executable file.
        MH_EXECUTE,

        /// Fixed VM shared library file.
        MH_FVMLIB,

        /// Core file.
        MH_CORE,

        /// Preloaded executable file.
        MH_PRELOAD,

        /// Dynamically bound shared library.
        MH_DYLIB,

        /// Dynamic link editor.
        MH_DYLINKER,

        /// Dynamically bound bundle file.
        MH_BUNDLE,

        /// Shared library stub for static linking only, no section contents.
        MH_DYLIB_STUB,

        /// Companion file with only debug sections.
        MH_DSYM,

        /// X86_64 kexts.
        MH_KEXT_BUNDLE
    }


    /// Constants for the flags field of the mach_header
    enum
    {
        /// The object file has no undefined references.
        MH_NOUNDEFS,

        /**
         * The object file is the output of an incremental link against a base
         * file and can't be link edited again.
         */
        MH_INCRLINK,

        /**
         * The object file is input for the dynamic linker and can't be
         * statically link edited again.
         */
        MH_DYLDLINK,

        /**
         * The object file's undefined references are bound by the dynamic
         * linker when loaded.
         */
        MH_BINDATLOAD,

        /// The file has its dynamic undefined references prebound.
        MH_PREBOUND,

        /// The file has its read-only and read-write segments split.
        MH_SPLIT_SEGS,

        /**
         * The shared library init routine is to be run lazily via catching
         * memory faults to its writeable segments (obsolete).
         */
        MH_LAZY_INIT,

        /// The image is using two-level name space bindings.
        MH_TWOLEVEL,

        /// The executable is forcing all images to use flat name space bindings.
        MH_FORCE_FLAT,

        /**
         * This umbrella guarantees no multiple definitions of symbols in its
         * sub-images so the two-level namespace hints can always be used.
         */
        MH_NOMULTIDEFS,

        /// Do not have dyld notify the prebinding agent about this executable.
        MH_NOFIXPREBINDING,

        /**
         * The binary is not prebound but can have its prebinding redone. only
         * used when MH_PREBOUND is not set.
         */
        MH_PREBINDABLE,

        /**
         * Indicates that this binary binds to all two-level namespace modules
         * of its dependent libraries. only used when MH_PREBINDABLE and
         * MH_TWOLEVEL are both set.
         */
        MH_ALLMODSBOUND,

        /**
         * Safe to divide up the sections into sub-sections via symbols for dead
         * code stripping.
         */
        MH_SUBSECTIONS_VIA_SYMBOLS,

        /// The binary has been canonicalized via the unprebind operation.
        MH_CANONICAL,

        /// The final linked image contains external weak symbols.
        MH_WEAK_DEFINES,

        /// The final linked image uses weak symbols.
        MH_BINDS_TO_WEAK,



        /**
         * When this bit is set, all stacks in the task will be given stack
         * execution privilege. Only used in MH_EXECUTE filetypes.
         */
        MH_ALLOW_STACK_EXECUTION,

        /**
         * When this bit is set, the binary declares it is safe for use in
         * processes with uid zero.
         */
        MH_ROOT_SAFE,



        /**
         * When this bit is set, the binary declares it is safe for use in
         * processes when issetugid() is true.
         */
        MH_SETUID_SAFE,



        /**
         * When this bit is set on a dylib, the static linker does not need to
         * examine dependent dylibs to see if any are re-exported.
         */
        MH_NO_REEXPORTED_DYLIBS,

        /**
         * When this bit is set, the OS will load the main executable at a
         * random address. Only used in MH_EXECUTE filetypes.
         */
        MH_PIE,

        /**
         * Only for use on dylibs. When linking against a dylib that has this
         * bit set, the static linker will automatically not create a
         * LC_LOAD_DYLIB load command to the dylib if no symbols are being
         * referenced from the dylib..
         */
        MH_DEAD_STRIPPABLE_DYLIB,

        /// Contains a section of type S_THREAD_LOCAL_VARIABLES.
        MH_HAS_TLV_DESCRIPTORS,



        /**
         * When this bit is set, the OS will run the main executable with a
         * non-executable heap even on platforms (e.g. i386) that don't require
         * it. Only used in MH_EXECUTE filetypes.
         */
        MH_NO_HEAP_EXECUTION,



        /// The code was linked for use in an application extension..
        MH_APP_EXTENSION_SAFE,



        /**
         * The external symbols listed in the nlist symbol table do not include
         * all the symbols listed in the dyld info.
         */
        MH_NLIST_OUTOFSYNC_WITH_DYLDINFO,

        /**
         * Allow LC_MIN_VERSION_MACOS and LC_BUILD_VERSION load commands with
         * the platforms macOS, iOSMac, iOSSimulator, tvOSSimulator and
         * watchOSSimulator.
         */
        MH_SIM_SUPPORT,

        /**
         * Only for use on dylibs. When this bit is set, the dylib is part of
         * the dyld shared cache, rather than loose in the filesystem.
         */
        MH_DYLIB_IN_CACHE
    }

    /**
     * The load commands directly follow the mach_header. The total size of all
     * of the commands is given by the sizeofcmds field in the mach_header. All
     * load commands must have as their first two fields cmd and cmdsize. The
     * cmd field is filled in with a constant for that command type. Each
     * command type has a structure specifically for it. The cmdsize field is
     * the size in bytes of the particular load command structure plus anything
     * that follows it that is a part of the load command
     * (i.e. section structures, strings, etc.). To advance to the next load
     * command the cmdsize can be added to the offset or pointer of the current
     * load command. The cmdsize for 32-bit architectures MUST be a multiple of
     * 4 bytes and for 64-bit architectures MUST be a multiple of 8 bytes
     * (these are forever the maximum alignment of any load commands). The
     * padded bytes must be zero. All tables in the object file must also
     * follow these rules so the file can be memory mapped. Otherwise the
     * pointers to these tables will not work well or at all on some machines.
     * With all padding zeroed like objects will compare byte for byte.
     */
    struct load_command
    {
        /// Type of load command.
        uint cmd;

        /// Total size of command in bytes.
        uint cmdsize;
    }

    /**
     * After MacOS X 10.1 when a new load command is added that is required to
     * be understood by the dynamic linker for the image to execute properly the
     * LC_REQ_DYLD bit will be or'ed into the load command constant. If the
     * dynamic linker sees such a load command it it does not understand will
     * issue a "unknown load command required for execution" error and refuse to
     * use the image. Other load commands without this bit that are not
     * understood will simply be ignored.
     */
    enum LC_REQ_DYLD;

    /// Constants for the cmd field of all load commands, the type.
    enum
    {
        /// Segment of this file to be mapped.
        LC_SEGMENT,

        /// Link-edit stab symbol table info.
        LC_SYMTAB,

        /// Link-edit gdb symbol table info (obsolete).
        LC_SYMSEG,

        /// Thread.
        LC_THREAD,

        /// Unix thread (includes a stack).
        LC_UNIXTHREAD,

        /// Load a specified fixed VM shared library.
        LC_LOADFVMLIB,

        /// Fixed VM shared library identification.
        LC_IDFVMLIB,

        /// Object identification info (obsolete).
        LC_IDENT,

        /// Fixed VM file inclusion (internal use).
        LC_FVMFILE,

        /// Prepage command (internal use).
        LC_PREPAGE,

        /// Dynamic link-edit symbol table info.
        LC_DYSYMTAB,

        /// Load a dynamically linked shared library.
        LC_LOAD_DYLIB,

        /// Dynamically linked shared lib ident.
        LC_ID_DYLIB,

        /// Load a dynamic linker.
        LC_LOAD_DYLINKER,

        /// Dynamic linker identification.
        LC_ID_DYLINKER,

        /// Modules prebound for a dynamically linked shared library.
        LC_PREBOUND_DYLIB,

        /// Image routines.
        LC_ROUTINES,

        /// Sub framework.
        LC_SUB_FRAMEWORK,

        /// Sub umbrella.
        LC_SUB_UMBRELLA,

        /// Sub client.
        LC_SUB_CLIENT,

        /// Sub library.
        LC_SUB_LIBRARY,

        /// Two-level namespace lookup hints.
        LC_TWOLEVEL_HINTS,

        /// Prebind checksum.
        LC_PREBIND_CKSUM
    }


    /**
     * Load a dynamically linked shared library that is allowed to be missing
     * (all symbols are weak imported).
     */
    ///
    enum LC_LOAD_WEAK_DYLIB;

    ///
    enum
    {
        /// 64-bit segment of this file to be mapped.
        LC_SEGMENT_64,

        /// 64-bit image routines.
        LC_ROUTINES_64,

        /// The uuid.
        LC_UUID,

        /// Runpath additions.
        LC_RPATH,

        /// Local of code signature.
        LC_CODE_SIGNATURE,

        /// Local of info to split segments.
        LC_SEGMENT_SPLIT_INFO,

        /// Load and re-export dylib.
        LC_REEXPORT_DYLIB,

        /// Delay load of dylib until first use.
        LC_LAZY_LOAD_DYLIB,

        /// Encrypted segment information.
        LC_ENCRYPTION_INFO,

        /// Compressed dyld information.
        LC_DYLD_INFO,

        /// Compressed dyld information only.
        LC_DYLD_INFO_ONLY,

        /// Load upward dylib.
        LC_LOAD_UPWARD_DYLIB,

        /// Build for MacOSX min OS version.
        LC_VERSION_MIN_MACOSX,

        /// Build for iPhoneOS min OS version.
        LC_VERSION_MIN_IPHONEOS,

        /// Compressed table of function start addresses.
        LC_FUNCTION_STARTS,

        /// String for dyld to treat like environment variable.
        LC_DYLD_ENVIRONMENT,

        /// Replacement for LC_UNIXTHREAD.
        LC_MAIN,

        /// Table of non-instructions in __text.
        LC_DATA_IN_CODE,

        /// Source version used to build binary.
        LC_SOURCE_VERSION,

        /// Code signing DRs copied from linked dylibs.
        LC_DYLIB_CODE_SIGN_DRS,

        /// 64-bit encrypted segment information.
        LC_ENCRYPTION_INFO_64,

        /// Linker options in MH_OBJECT files.
        LC_LINKER_OPTION,

        /// Optimization hints in MH_OBJECT files.
        LC_LINKER_OPTIMIZATION_HINT,

        /// Build for AppleTV min OS version.
        LC_VERSION_MIN_TVOS,

        /// Build for Watch min OS version.
        LC_VERSION_MIN_WATCHOS,

        /// Arbitrary data included within a Mach-O file.
        LC_NOTE,

        /// Build for platform min OS version.
        LC_BUILD_VERSION,

        /// Used with linkedit_data_command, payload is trie.
        LC_DYLD_EXPORTS_TRIE,

        /// Used with linkedit_data_command.
        LC_DYLD_CHAINED_FIXUPS
    }


    /**
     * A variable length string in a load command is represented by an lc_str
     * union. The strings are stored just after the load command structure and
     * the offset is from the start of the load command structure. The size
     * of the string is reflected in the cmdsize field of the load command.
     * Once again any padded bytes to bring the cmdsize field to a multiple
     * of 4 bytes must be zero.
     */
    union lc_str
    {
        /// Offset to the string.
        uint offset;

        /// Pointer to the string (only available on non 64 bit platforms).
        char* ptr;
    }

    /**
     * The segment load command indicates that a part of this file is to be
     * mapped into the task's address space. The size of this segment in memory,
     * vmsize, maybe equal to or larger than the amount to map from this file,
     * filesize. The file is mapped starting at fileoff to the beginning of
     * the segment in memory, vmaddr. The rest of the memory of the segment,
     * if any, is allocated zero fill on demand. The segment's maximum virtual
     * memory protection and initial virtual memory protection are specified
     * by the maxprot and initprot fields. If the segment has sections then the
     * section structures directly follow the segment command and their size is
     * reflected in cmdsize.
     */
    struct segment_command
    {
        /// LC_SEGMENT.
        uint cmd;

        /// Includes sizeof section structs.
        uint cmdsize;

        /// Segment name.
        char[16] segname;

        /// Memory address of this segment.
        uint vmaddr;

        /// Memory size of this segment.
        uint vmsize;

        /// File offset of this segment.
        uint fileoff;

        /// Amount to map from the file.
        uint filesize;

        /// Maximum VM protection.
        int maxprot;

        /// Initial VM protection.
        int initprot;

        /// Number of sections in segment.
        uint nsects;

        /// Flags.
        uint flags;
    }

    /*
     * The 64-bit segment load command indicates that a part of this file is to
     * be mapped into a 64-bit task's address space. If the 64-bit segment has
     * sections then section_64 structures directly follow the 64-bit segment
     * command and their size is reflected in cmdsize.
     */
    struct segment_command_64
    {
        /// LC_SEGMENT_64.
        uint cmd;

        /// Includes sizeof section_64 structs.
        uint cmdsize;

        /// Segment name.
        char[16] segname;

        /// Memory address of this segment.
        ulong vmaddr;

        /// Memory size of this segment.
        ulong vmsize;

        /// File offset of this segment.
        ulong fileoff;

        /// Amount to map from the file.
        ulong filesize;

        /// Maximum VM protection.
        int maxprot;

        /// Initial VM protection.
        int initprot;

        /// Number of sections in segment.
        uint nsects;

        /// Flags.
        uint flags;
    }

    /// Constants for the flags field of the segment_command.
    enum
    {
        /**
         * The file contents for this segment is for the high part of the VM
         * space, the low part is zero filled (for stacks in core files).
         */
        SG_HIGHVM,

        /**
         * This segment is the VM that is allocated by a fixed VM library,
         * for overlap checking in the link editor.
         */
        SG_FVMLIB,

        /**
         * This segment has nothing that was relocated in it and nothing
         * relocated to it, that is it maybe safely replaced without relocation.
         */
        SG_NORELOC,

        /**
         * This segment is protected.
         *
         * If the segment starts at file offset 0, the first page of the segment
         * is not protected. All other pages of the segment are protected.
         */
        SG_PROTECTED_VERSION_1,

        /// This segment is made read-only after fixups.
        SG_READ_ONLY
    }


    /**
     * A segment is made up of zero or more sections. Non-MH_OBJECT files have
     * all of their segments with the proper sections in each, and padded to the
     * specified segment alignment when produced by the link editor. The first
     * segment of a MH_EXECUTE and MH_FVMLIB format file contains the
     * mach_header and load commands of the object file before its first
     * section. The zero fill sections are always last in their segment
     * (in all formats). This allows the zeroroed segment padding to be mapped
     * into memory where zero fill sections might be. The gigabyte zero fill
     * sections, those with the section type S_GB_ZEROFILL, can only be in a
     * segment with sections of this type. These segments are then placed after
     * all other segments.
     *
     * The MH_OBJECT format has all of its sections in one segment for
     * compactness. There is no padding to a specified segment boundary and the
     * mach_header and load commands are not part of the segment.
     *
     * Sections with the same section name, sectname, going into the same
     * segment, segname, are combined by the link editor. The resulting section,
     * is aligned to the maximum alignment of the combined sections and is the
     * new section's alignment. The combined sections are aligned to their
     * original alignment in the combined section. Any padded bytes to get the
     * specified alignment are zeroed.
     *
     * The format of the relocation entries referenced by the reloff and nreloc
     * fields of the section structure for mach object files is described in the
     * header file <reloc.h>.
     */
    struct section
    {
        /// Name of this section.
        char[16] sectname;

        /// Segment this section goes in.
        char[16] segname;

        /// Memory address of this section.
        uint addr;

        /// Size in bytes of this section.
        uint size;

        /// File offset of this section.
        uint offset;

        /// Section alignment (power of 2).
        uint align_;

        /// File offset of relocation entries.
        uint reloff;

        /// Number of relocation entries.
        uint nreloc;

        /// Flags (section type and attributes).
        uint flags;

        /// Reserved (for offset or index).
        uint reserved1;

        /// Reserved (for count or sizeof).
        uint reserved2;
    }

    ///
    struct section_64
    {
        /// Name of this section.
        char[16] sectname;

        /// Segment this section goes in.
        char[16] segname;

        /// Memory address of this section.
        ulong addr;

        /// Size in bytes of this section.
        ulong size;

        /// File offset of this section.
        uint offset;

        /// Section alignment (power of 2).
        uint align_;

        /// File offset of relocation entries.
        uint reloff;

        /// Number of relocation entries.
        uint nreloc;

        /// Flags (section type and attributes).
        uint flags;

        /// Reserved (for offset or index).
        uint reserved1;

        /// Reserved (for count or sizeof).
        uint reserved2;

        /// Reserved.
        uint reserved3;
    }

    /**
     * The flags field of a section structure is separated into two parts a section
     * type and section attributes. The section types are mutually exclusive (it
     * can only have one type) but the section attributes are not (it may have more
     * than one attribute).
     */
    enum
    {
        /// 256 section types.
        SECTION_TYPE,

        /// 24 section attributes.
        SECTION_ATTRIBUTES
    }

    /// Constants for the type of a section.
    enum
    {
        /// Regular section.
        S_REGULAR,

        /// Zero fill on demand section.
        S_ZEROFILL,

        /// Section with only literal C strings.
        S_CSTRING_LITERALS,

        /// Section with only 4 byte literals.
        S_4BYTE_LITERALS,

        /// Section with only 8 byte literals.
        S_8BYTE_LITERALS,

        /// Section with only pointers to literals.
        S_LITERAL_POINTERS,

        /**
         * Section with only non-lazy symbol pointers.
         *
         * For the two types of symbol pointers sections and the symbol stubs
         * section they have indirect symbol table entries. For each of the
         * entries in the section the indirect symbol table entries, in
         * corresponding order in the indirect symbol table, start at the index
         * stored in the reserved1 field of the section structure. Since the
         * indirect symbol table entries correspond to the entries in the
         * section the number of indirect symbol table entries is inferred from
         * the size of the section divided by the size of the entries in the
         * section. For symbol pointers sections the size of the entries in the
         * section is 4 bytes and for symbol stubs sections the byte size of the
         * stubs is stored in the reserved2 field of the section structure.
         */
        S_NON_LAZY_SYMBOL_POINTERS,

        /// Section with only lazy symbol pointers.
        S_LAZY_SYMBOL_POINTERS,

        /// Section with only symbol stubs, byte size of stub in the reserved2 field.
        S_SYMBOL_STUBS,

        /// Section with only function pointers for initialization.
        S_MOD_INIT_FUNC_POINTERS,

        /// Section with only function pointers for termination.
        S_MOD_TERM_FUNC_POINTERS,

        /// Section contains symbols that are to be coalesced.
        S_COALESCED,

        /// Zero fill on demand section (that can be larger than 4 gigabytes).
        S_GB_ZEROFILL,

        /// Section with only pairs of function pointers for interposing.
        S_INTERPOSING,

        /// Section with only 16 byte literals.
        S_16BYTE_LITERALS,

        /// Section contains DTrace Object Format.
        S_DTRACE_DOF,

        /// Section with only lazy symbol pointers to lazy loaded dylibs.
        S_LAZY_DYLIB_SYMBOL_POINTERS,



        // Section types to support thread local variables.

        /// Template of initial values for TLVs.
        S_THREAD_LOCAL_REGULAR,

        /// Template of initial values for TLVs.
        S_THREAD_LOCAL_ZEROFILL,

        /// TLV descriptors.
        S_THREAD_LOCAL_VARIABLES,

        /// Pointers to TLV descriptors.
        S_THREAD_LOCAL_VARIABLE_POINTERS,

        /// Functions to call to initialize TLV values.
        S_THREAD_LOCAL_INIT_FUNCTION_POINTERS,

        /// 32-bit offsets to initializers.
        S_INIT_FUNC_OFFSETS
    }

    /**
     * Constants for the section attributes part of the flags field of a section
     * structure.
     */
    enum
    {
        /// User setable attributes.
        SECTION_ATTRIBUTES_USR,

        /// Section contains only true machine instructions.
        S_ATTR_PURE_INSTRUCTIONS,

        /// Section contains coalesced symbols that are not to be in a ranlib table of contents.
        S_ATTR_NO_TOC,

        /// Ok to strip static symbols in this section in files with the MH_DYLDLINK flag.
        S_ATTR_STRIP_STATIC_SYMS,

        /// No dead stripping.
        S_ATTR_NO_DEAD_STRIP,

        /// Blocks are live if they reference live blocks.
        S_ATTR_LIVE_SUPPORT,

        /// Used with i386 code stubs written on by dyld.
        S_ATTR_SELF_MODIFYING_CODE,

        /**
         * A debug section.
         *
         * If a segment contains any sections marked with S_ATTR_DEBUG then all
         * sections in that segment must have this attribute. No section other
         * than a section marked with this attribute may reference the contents
         * of this section. A section with this attribute may contain no symbols
         * and must have a section type S_REGULAR. The static linker will not
         * copy section contents from sections with this attribute into its
         * output file. These sections generally contain DWARF debugging info.
         */
        S_ATTR_DEBUG,

        /// System setable attributes.
        SECTION_ATTRIBUTES_SYS,

        /// Section contains some machine instructions.
        S_ATTR_SOME_INSTRUCTIONS,

        /// Section has external relocation entries.
        S_ATTR_EXT_RELOC,

        /// Section has local relocation entries.
        S_ATTR_LOC_RELOC
    }

    /**
     * The names of segments and sections in them are mostly meaningless to the
     * link-editor. But there are few things to support traditional UNIX
     * executables that require the link-editor and assembler to use some names
     * agreed upon by convention.
     *
     * The initial protection of the "__TEXT" segment has write protection
     * turned off (not writeable).
     *
     * The link-editor will allocate common symbols at the end of the "__common"
     * section in the "__DATA" segment. It will create the section and segment
     * if needed.
     *
     * The currently known segment names and the section names in those segments.
     */
    enum
    {
        /**
         * The pagezero segment which has no protections and catches NULL
         * references for MH_EXECUTE files.
         */
        SEG_PAGEZERO,



        /// The tradition UNIX text segment.
        SEG_TEXT,

        /// The real text part of the text section no headers, and no padding.
        SECT_TEXT,

        /// The fvmlib initialization section.
        SECT_FVMLIB_INIT0,

        /// The section following the fvmlib initialization section.
        SECT_FVMLIB_INIT1,



        /// The tradition UNIX data segment.
        SEG_DATA,

        /// The real initialized data section no padding, no bss overlap.
        SECT_DATA,

        /// The real uninitialized data section no padding.
        SECT_BSS,

        /// The section common symbols are allocated in by the link editor.
        SECT_COMMON,



        /// Objective-C runtime segment.
        SEG_OBJC,

        /// Symbol table.
        SECT_OBJC_SYMBOLS,

        /// Module information.
        SECT_OBJC_MODULES,

        /// String table.
        SECT_OBJC_STRINGS,

        /// String table.
        SECT_OBJC_REFS,



        /// The icon segment.
        SEG_ICON,

        /// The icon headers.
        SECT_ICON_HEADER,

        /// The icons in tiff format.
        SECT_ICON_TIFF,



        /**
         * The segment containing all structs created and maintained by the link
         * editor. Created with -seglinkedit option to ld(1) for MH_EXECUTE and
         * FVMLIB file types only.
         */
        SEG_LINKEDIT,


        /// The unix stack segment.
        SEG_UNIXSTACK,


        /**
         * The segment for the self (dyld) modifing code stubs that has read,
         * write and execute permissions.
         */
        SEG_IMPORT
    }

    /**
     * Fixed virtual memory shared libraries are identified by two things. The
     * target pathname (the name of the library as found for execution), and the
     * minor version number. The address of where the headers are loaded is in
     * header_addr. (THIS IS OBSOLETE and no longer supported).
     */
    struct fvmlib
    {
        /// Library's target pathname.
        lc_str name;

        /// Library's minor version number.
        uint minor_version;

        /// Library's header address.
        uint header_addr;
    }

    /**
     * A fixed virtual shared library (filetype == MH_FVMLIB in the mach header)
     * contains a fvmlib_command (cmd == LC_IDFVMLIB) to identify the library.
     * An object that uses a fixed virtual shared library also contains a
     * fvmlib_command (cmd == LC_LOADFVMLIB) for each library it uses.
     * (THIS IS OBSOLETE and no longer supported).
     */
    struct fvmlib_command
    {
        /// LC_IDFVMLIB or LC_LOADFVMLIB.
        uint cmd;

        /// Includes pathname string.
        uint cmdsize;

        /// The library identification.
        fvmlib fvmlib_;
    }

    /**
     * Dynamically linked shared libraries are identified by two things. The
     * pathname (the name of the library as found for execution), and the
     * compatibility version number. The pathname must match and the
     * compatibility number in the user of the library must be greater than or
     * equal to the library being used. The time stamp is used to record the
     * time a library was built and copied into user so it can be use to
     * determined if the library used at runtime is exactly the same as used to
     * built the program.
     */
    struct dylib
    {
        /// Library's path name.
        lc_str name;

        /// Library's build time stamp.
        uint timestamp;

        /// Library's current version number.
        uint current_version;

        /// Library's compatibility version number.
        uint compatibility_version;
    }

    /**
     * A dynamically linked shared library
     * (filetype == MH_DYLIB in the mach header) contains a dylib_command
     * (cmd == LC_ID_DYLIB) to identify the library. An object that uses a
     * dynamically linked shared library also contains a dylib_command
     * (cmd == LC_LOAD_DYLIB, LC_LOAD_WEAK_DYLIB, or LC_REEXPORT_DYLIB) for each
     * library it uses.
     */
    struct dylib_command
    {
        /// LC_ID_DYLIB, LC_LOAD_{,WEAK_}DYLIB, LC_REEXPORT_DYLIB.
        uint cmd;

        /// Includes pathname string.
        uint cmdsize;

        /// The library identification.
        dylib dylib_;
    }

    /**
     * A dynamically linked shared library may be a subframework of an umbrella
     * framework. If so it will be linked with "-umbrella umbrella_name" where
     * Where "umbrella_name" is the name of the umbrella framework. A
     * subframework can only be linked against by its umbrella framework or
     * other subframeworks that are part of the same umbrella framework.
     * Otherwise the static link editor produces an error and states to link
     * against the umbrella framework. The name of the umbrella framework for
     * subframeworks is recorded in the following structure.
     */
    struct sub_framework_command
    {
        /// LC_SUB_FRAMEWORK.
        uint cmd;

        /// Includes umbrella string.
        uint cmdsize;

        /// The umbrella framework name.
        lc_str umbrella;
    }

    /**
     * For dynamically linked shared libraries that are subframework of an
     * umbrella framework they can allow clients other than the umbrella
     * framework or other subframeworks in the same umbrella framework. To do
     * this the subframework is built with "-allowable_client client_name" and
     * an LC_SUB_CLIENT load command is created for each -allowable_client flag.
     * The client_name is usually a framework name. It can also be a name used
     * for bundles clients where the bundle is built with
     * "-client_name client_name".
     */
    struct sub_client_command
    {
        /// LC_SUB_CLIENT.
        uint cmd;

        /// Includes client string.
        uint cmdsize;

        /// The client name.
        lc_str client;
    }

    /**
     * A dynamically linked shared library may be a sub_umbrella of an umbrella
     * framework. If so it will be linked with "-sub_umbrella umbrella_name"
     * where "umbrella_name" is the name of the sub_umbrella framework. When
     * statically linking when -twolevel_namespace is in effect a twolevel
     * namespace umbrella framework will only cause its subframeworks and those
     * frameworks listed as sub_umbrella frameworks to be implicited linked in.
     * Any other dependent dynamic libraries will not be linked it when
     * -twolevel_namespace is in effect. The primary library recorded by the
     * static linker when resolving a symbol in these libraries will be the
     * umbrella framework. Zero or more sub_umbrella frameworks may be use by an
     * umbrella framework. The name of a sub_umbrella framework is recorded in
     * the following structure.
     */
    struct sub_umbrella_command
    {
        /// LC_SUB_UMBRELLA.
        uint cmd;

        /// Includes sub_umbrella string.
        uint cmdsize;

        /// The sub_umbrella framework name.
        lc_str sub_umbrella;
    }

    /**
     * A dynamically linked shared library may be a sub_library of another
     * shared library. If so it will be linked with "-sub_library library_name"
     * where "library_name" is the name of the sub_library shared library. When
     * statically linking when -twolevel_namespace is in effect a twolevel
     * namespace shared library will only cause its subframeworks and those
     * frameworks listed as sub_umbrella frameworks and libraries listed as
     * sub_libraries to be implicited linked in. Any other dependent dynamic
     * libraries will not be linked it when -twolevel_namespace is in effect.
     * The primary library recorded by the static linker when resolving a symbol
     * in these libraries will be the umbrella framework (or dynamic library).
     * Zero or more sub_library shared libraries may be use by an umbrella
     * framework or (or dynamic library). The name of a sub_library framework is
     * recorded in the following structure. For example
     * /usr/lib/libobjc_profile.A.dylib would be recorded as "libobjc".
     */
    struct sub_library_command
    {
        /// LC_SUB_LIBRARY.
        uint cmd;

        /// Includes sub_library string.
        uint cmdsize;

        /// The sub_library name.
        lc_str sub_library;
    }

    /**
     * A program (filetype == MH_EXECUTE) that is
     * prebound to its dynamic libraries has one of these for each library that
     * the static linker used in prebinding. It contains a bit vector for the
     * modules in the library. The bits indicate which modules are bound (1) and
     * which are not (0) from the library. The bit for module 0 is the low bit
     * of the first byte. So the bit for the Nth module is:
     * (linked_modules[N/8] >> N%8) & 1
     */
    struct prebound_dylib_command
    {
        /// LC_PREBOUND_DYLIB.
        uint cmd;

        /// Includes strings.
        uint cmdsize;

        /// Library's path name.
        lc_str name;

        /// Number of modules in library.
        uint nmodules;

        /// Bit vector of linked modules.
        lc_str linked_modules;
    }

    /**
     * A program that uses a dynamic linker contains a dylinker_command to
     * identify the name of the dynamic linker (LC_LOAD_DYLINKER). And a dynamic
     * linker contains a dylinker_command to identify the dynamic linker
     * (LC_ID_DYLINKER). A file can have at most one of these.
     * This struct is also used for the LC_DYLD_ENVIRONMENT load command and
     * contains string for dyld to treat like environment variable.
     */
    struct dylinker_command
    {
        /// LC_ID_DYLINKER, LC_LOAD_DYLINKER or LC_DYLD_ENVIRONMENT.
        uint cmd;

        /// Includes pathname string.
        uint cmdsize;

        /// Dynamic linker's path name.
        lc_str name;
    }

    /**
     * Thread commands contain machine-specific data structures suitable for
     * use in the thread state primitives. The machine specific data structures
     * follow the struct thread_command as follows.
     * Each flavor of machine specific data structure is preceded by an uint32_t
     * constant for the flavor of that data structure, an uint32_t that is the
     * count of uint32_t's of the size of the state data structure and then
     * the state data structure follows. This triple may be repeated for many
     * flavors. The constants for the flavors, counts and state data structure
     * definitions are expected to be in the header file <machine/thread_status.h>.
     * These machine specific data structures sizes must be multiples of
     * 4 bytes. The cmdsize reflects the total size of the thread_command
     * and all of the sizes of the constants for the flavors, counts and state
     * data structures.
     *
     * For executable objects that are unix processes there will be one
     * thread_command (cmd == LC_UNIXTHREAD) created for it by the link-editor.
     * This is the same as a LC_THREAD, except that a stack is automatically
     * created (based on the shell's limit for the stack size). Command
     * arguments and environment variables are copied onto that stack.
     */
    struct thread_command
    {
        /// LC_THREAD or  LC_UNIXTHREAD.
        uint cmd;

        /// Total size of this command.
        uint cmdsize;
    }

    /**
     * The routines command contains the address of the dynamic shared library
     * initialization routine and an index into the module table for the module
     * that defines the routine. Before any modules are used from the library
     * the dynamic linker fully binds the module that defines the initialization
     * routine and then calls it. This gets called before any module
     * initialization routines (used for C++ static constructors) in the library.
     */
    struct routines_command
    {
        /// LC_ROUTINES.
        uint cmd;

        /// Total size of this command.
        uint cmdsize;

        /// Address of initialization routine.
        uint init_address;

        /// Index into the module table that.
        uint init_module;

        // the init routine is defined in

        ///
        uint reserved1;

        ///
        uint reserved2;

        ///
        uint reserved3;

        ///
        uint reserved4;

        ///
        uint reserved5;

        ///
        uint reserved6;
    }

    /// The 64-bit routines command. Same use as above.
    struct routines_command_64
    {
        /// LC_ROUTINES_64.
        uint cmd;

        /// Total size of this command.
        uint cmdsize;

        /// Address of initialization routine.
        ulong init_address;

        /// Index into the module table that.
        ulong init_module;

        /*  the init routine is defined in */

        ///
        ulong reserved1;

        ///
        ulong reserved2;

        ///
        ulong reserved3;

        ///
        ulong reserved4;

        ///
        ulong reserved5;

        ///
        ulong reserved6;
    }

    /**
     * The symtab_command contains the offsets and sizes of the link-edit 4.3BSD
     * "stab" style symbol table information as described in the header files
     * <nlist.h> and <stab.h>.
     */
    struct symtab_command
    {
        /// LC_SYMTAB.
        uint cmd;

        /// Sizeof(struct symtab_command).
        uint cmdsize;

        /// Symbol table offset.
        uint symoff;

        /// Number of symbol table entries.
        uint nsyms;

        /// String table offset.
        uint stroff;

        /// String table size in bytes.
        uint strsize;
    }

    /**
     * This is the second set of the symbolic information which is used to
     * support the data structures for the dynamically link editor.
     *
     * The original set of symbolic information in the symtab_command which contains
     * the symbol and string tables must also be present when this load command is
     * present. When this load command is present the symbol table is organized
     * into three groups of symbols:
     * * local symbols (static and debugging symbols) - grouped by module
     * * defined external symbols - grouped by module (sorted by name if not lib)
     * * undefined external symbols (sorted by name if MH_BINDATLOAD is not set,
     *      and in order the were seen by the static
     *      linker if MH_BINDATLOAD is set)
     *
     * In this load command there are offsets and counts to each of the three
     * groups of symbols.
     *
     * This load command contains a the offsets and sizes of the following new
     * symbolic information tables:
     * * table of contents
     * * module table
     * * reference symbol table
     * * indirect symbol table
     *
     * The first three tables above (the table of contents, module table and
     * reference symbol table) are only present if the file is a dynamically
     * linked shared library. For executable and object modules, which are files
     * containing only one module, the information that would be in these three
     * tables is determined as follows:
     * * table of contents - the defined external symbols are sorted by name
     * * module table - the file contains only one module so everything in the
     *      file is part of the module.
     * * reference symbol table - is the defined and undefined external symbols
     *
     * For dynamically linked shared library files this load command also
     * contains offsets and sizes to the pool of relocation entries for all
     * sections separated into two groups:
     * * external relocation entries
     * * local relocation entries
     *
     * For executable and object modules the relocation entries continue to hang
     * off the section structures.
     */
    struct dysymtab_command
    {
        /// LC_DYSYMTAB.
        uint cmd;

        /// sizeof(struct dysymtab_command).
        uint cmdsize;

        /**
         * Index to local symbols.
         *
         * The symbols indicated by symoff and nsyms of the LC_SYMTAB load command
         * are grouped into the following three groups:
         * * local symbols (further grouped by the module they are from)
         * * defined external symbols (further grouped by the module they are from)
         * * undefined symbols
         *
         * The local symbols are used only for debugging. The dynamic binding
         * process may have to use them to indicate to the debugger the local
         * symbols for a module that is being bound.
         *
         * The last two groups are used by the dynamic binding process to do the
         * binding (indirectly through the module table and the reference symbol
         * table when this is a dynamically linked shared library file).
         */
        uint ilocalsym;

        /// Number of local symbols.
        uint nlocalsym;



        /// Index to externally defined symbols.
        uint iextdefsym;

        /// Number of externally defined symbols.
        uint nextdefsym;



        /// Index to undefined symbols.
        uint iundefsym;

        /// Number of undefined symbols.
        uint nundefsym;

        /**
         * File offset to table of contents.
         *
         * For the for the dynamic binding process to find which module a symbol
         * is defined in the table of contents is used (analogous to the ranlib
         * structure in an archive) which maps defined external symbols to
         * modules they are defined in. This exists only in a dynamically linked
         * shared library file. For executable and object modules the defined
         * external symbols are sorted by name and is use as the table of
         * contents.
         */
        uint tocoff;

        /// Number of entries in table of contents.
        uint ntoc;

        /**
         * File offset to module table.
         *
         * To support dynamic binding of "modules" (whole object files) the
         * symbol table must reflect the modules that the file was created from.
         * This is done by having a module table that has indexes and counts
         * into the merged tables for each module. The module structure that
         * these two entries refer to is described below. This exists only in a
         * dynamically linked shared library file. For executable and object
         * modules the file only contains one module so everything in the file
         * belongs to the module.
         */
        uint modtaboff;

        /// Number of module table entries.
        uint nmodtab;

        /**
         * Offset to referenced symbol table.
         *
         * To support dynamic module binding the module structure for each
         * module indicates the external references (defined and undefined) each
         * module makes. For each module there is an offset and a count into the
         * reference symbol table for the symbols that the module references.
         * This exists only in a dynamically linked shared library file. For
         * executable and object modules the defined external symbols and the
         * undefined external symbols indicates the external references.
         */
        uint extrefsymoff;

        /// Number of referenced symbol table entries.
        uint nextrefsyms;

        /**
         * File offset to the indirect symbol table.
         *
         * The sections that contain "symbol pointers" and "routine stubs" have
         * indexes and (implied counts based on the size of the section and
         * fixed size of the entry) into the "indirect symbol" table for each
         * pointer and stub. For every section of these two types the index into
         * the indirect symbol table is stored in the section header in the
         * field reserved1. An indirect symbol table entry is simply a 32bit
         * index into the symbol table to the symbol that the pointer or stub is
         * referring to. The indirect symbol table is ordered to match the
         * entries in the section.
         */
        uint indirectsymoff;

        /// Number of indirect symbol table entries.
        uint nindirectsyms;

        /**
         * Offset to external relocation entries-
         *
         * To support relocating an individual module in a library file quickly
         * the external relocation entries for each module in the library need
         * to be accessed efficiently. Since the relocation entries can't be
         * accessed through the section headers for a library file they are
         * separated into groups of local and external entries further grouped
         * by module. In this case the presents of this load command who's
         * extreloff, nextrel, locreloff and nlocrel fields are non-zero
         * indicates that the relocation entries of non-merged sections are not
         * referenced through the section structures (and the reloff and nreloc
         * fields in the section headers are set to zero).
         *
         * Since the relocation entries are not accessed through the section
         * headers this requires the r_address field to be something other than
         * a section offset to identify the item to be relocated. In this case
         * r_address is set to the offset from the vmaddr of the first
         * LC_SEGMENT command. For MH_SPLIT_SEGS images r_address is set to the
         * offset from thevmaddr of the first read-write LC_SEGMENT command.
         *
         * The relocation entries are grouped by module and the module table
         * entries have indexes and counts into them for the group of external
         * relocation entries for that the module.
         *
         * For sections that are merged across modules there must not be any
         * remaining external relocation entries for them (for merged sections
         * remaining relocation entries must be local).
         */
        uint extreloff;

        /// Number of external relocation entries.
        uint nextrel;

        /**
         * Offset to local relocation entries.
         *
         * All the local relocation entries are grouped together (they are not
         * grouped by their module since they are only used if the object is
         * moved from it statically link edited address).
         */
        uint locreloff;

        /// Number of local relocation entries.
        uint nlocrel;
    }

    /**
     * An indirect symbol table entry is simply a 32bit index into the symbol
     * table to the symbol that the pointer or stub is referring to. Unless it
     * is for a non-lazy symbol pointer section for a defined symbol which
     * strip(1) as removed. In which case it has the value
     * INDIRECT_SYMBOL_LOCAL. If the symbol was also absolute
     * INDIRECT_SYMBOL_ABS is or'ed with that.
     */
    enum
    {
        ///
        INDIRECT_SYMBOL_LOCAL,

        ///
        INDIRECT_SYMBOL_ABS
    }

    /// A table of contents entry.
    struct dylib_table_of_contents
    {
        /// The defined external symbol (index into the symbol table).
        uint symbol_index;

        /// Index into the module table this symbol is defined in.
        uint module_index;
    }

    /// A module table entry.
    struct dylib_module
    {
        /// The module name (index into string table).
        uint module_name;



        /// Index into externally defined symbols.
        uint iextdefsym;

        /// Number of externally defined symbols.
        uint nextdefsym;

        /// Index into reference symbol table.
        uint irefsym;

        /// Number of reference symbol table entries.
        uint nrefsym;

        /// Index into symbols for local symbols.
        uint ilocalsym;

        /// Number of local symbols.
        uint nlocalsym;



        /// Index into external relocation entries.
        uint iextrel;

        /// Number of external relocation entries.
        uint nextrel;



        /**
         * Low 16 bits are the index into the init section, high 16 bits are the
         * index into the term section.
         */
        uint iinit_iterm;

        /**
         * Low 16 bits are the number of init section entries, high 16 bits are
         * the number of term section entries.
         */
        uint ninit_nterm;



        /**
         * The (__OBJC,__module_info) section.
         *
         * For this module address of the start of.
         */
        uint objc_module_info_addr;

        /**
         * The (__OBJC,__module_info) section.
         *
         * For this module size of.
         */
        uint objc_module_info_size;
    }

    /// A 64-bit module table entry.
    struct dylib_module_64
    {
        /// The module name (index into string table).
        uint module_name;



        /// Index into externally defined symbols.
        uint iextdefsym;

        /// Number of externally defined symbols.
        uint nextdefsym;

        /// Index into reference symbol table.
        uint irefsym;

        /// Number of reference symbol table entries.
        uint nrefsym;

        /// Index into symbols for local symbols.
        uint ilocalsym;

        /// Number of local symbols.
        uint nlocalsym;



        /// Index into external relocation entries.
        uint iextrel;

        /// Number of external relocation entries.
        uint nextrel;



        /**
         * Low 16 bits are the index into the init section, high 16 bits are the
         * index into the term section.
         */
        uint iinit_iterm;

        /**
         * Low 16 bits are the number of init section entries, high 16 bits are
         * the number of term section entries.
         */
        uint ninit_nterm;



        /**
         * The (__OBJC,__module_info) section.
         *
         * For this module size of.
         */
        uint objc_module_info_size;

        /**
         * The (__OBJC,__module_info) section.
         *
         * For this module address of the start of.
         */
        ulong objc_module_info_addr;
    }

    /**
     * The entries in the reference symbol table are used when loading the
     * module (both by the static and dynamic link editors) and if the module is
     * unloaded or replaced. Therefore all external symbols
     * (defined and undefined) are listed in the module's reference table. The
     * flags describe the type of reference that is being made. The constants
     * for the flags are defined in <mach-o/nlist.h> as they are also used for
     * symbol table entries.
     */
    struct dylib_reference
    {
        /// Index into the symbol table.
        @property uint isym() const pure nothrow @nogc @safe;

        /// ditto
        @property void isym(uint v) @safe pure nothrow @nogc;

        /// Flags to indicate the type of reference.
        @property uint flags() const pure nothrow @nogc @safe;

        /// ditto
        @property void flags(uint v) pure nothrow @nogc @safe;
    }

    /**
     * The twolevel_hints_command contains the offset and number of hints in the
     * two-level namespace lookup hints table.
     */
    struct twolevel_hints_command
    {
        /// LC_TWOLEVEL_HINTS.
        uint cmd;

        /// Sizeof(struct twolevel_hints_command).
        uint cmdsize;

        /// Offset to the hint table.
        uint offset;

        /// Number of hints in the hint table.
        uint nhints;
    }

    /**
     * The entries in the two-level namespace lookup hints table are
     * twolevel_hint structs. These provide hints to the dynamic link editor
     * where to start looking for an undefined symbol in a two-level namespace
     * image. The isub_image field is an index into the sub-images
     * (sub-frameworks and sub-umbrellas list) that made up the two-level image
     * that the undefined symbol was found in when it was built by the static
     * link editor. If isub-image is 0 the symbol is expected to be defined
     * in library and not in the sub-images. If isub-image is non-zero it is an
     * index into the array of sub-images for the umbrella with the first index
     * in the sub-images being 1. The array of sub-images is the ordered list of
     * sub-images of the umbrella that would be searched for a symbol that has
     * the umbrella recorded as its primary library. The table of contents index
     * is an index into the library's table of contents. This is used as the
     * starting point of the binary search or a directed linear search.
     */
    struct twolevel_hint
    {
        /// Index into the sub images.
        @property uint isub_image() const pure nothrow @nogc @safe;

        /// ditto
        @property void isub_image(uint v) pure nothrow @nogc @safe;

        /// Index into the table of contents.
        @property uint itoc() const pure nothrow @nogc @safe;

        /// ditto
        @property void itoc(uint v) pure nothrow @nogc @safe;
    }

    /**
     * The prebind_cksum_command contains the value of the original check sum
     * for prebound files or zero. When a prebound file is first created or
     * modified for other than updating its prebinding information the value of
     * the check sum is set to zero. When the file has it prebinding re-done and
     * if the value of the check sum is zero the original check sum is
     * calculated and stored in cksum field of this load command in the output
     * file. If when the prebinding is re-done and the cksum field is non-zero
     * it is left unchanged from the input file.
     */
    struct prebind_cksum_command
    {
        /// LC_PREBIND_CKSUM.
        uint cmd;

        /// Sizeof(struct prebind_cksum_command).
        uint cmdsize;

        /// The check sum or zero.
        uint cksum;
    }

    /**
     * The uuid load command contains a single 128-bit unique random number that
     * identifies an object produced by the static link editor.
     */
    struct uuid_command
    {
        /// LC_UUID.
        uint cmd;

        /// Sizeof(struct uuid_command).
        uint cmdsize;

        /// The 128-bit uuid.
        ubyte[16] uuid;
    }

    /**
     * The rpath_command contains a path which at runtime should be added to
     * the current run path used to find @rpath prefixed dylibs.
     */
    struct rpath_command
    {
        /// LC_RPATH.
        uint cmd;

        /// Includes string.
        uint cmdsize;

        /// Path to add to run path.
        lc_str path;
    }

    /**
     * The linkedit_data_command contains the offsets and sizes of a blob
     * of data in the __LINKEDIT segment.
     */
    struct linkedit_data_command
    {
        /**
         * LC_CODE_SIGNATURE, LC_SEGMENT_SPLIT_INFO, LC_FUNCTION_STARTS,
         * LC_DATA_IN_CODE, LC_DYLIB_CODE_SIGN_DRS,
         * LC_LINKER_OPTIMIZATION_HINT, LC_DYLD_EXPORTS_TRIE or
         * LC_DYLD_CHAINED_FIXUPS.
         */
        uint cmd;

        /// Sizeof(struct linkedit_data_command).
        uint cmdsize;

        /// File offset of data in __LINKEDIT segment.
        uint dataoff;

        /// File size of data in __LINKEDIT segment.
        uint datasize;

    }

    /**
     * The encryption_info_command contains the file offset and size of an
     * of an encrypted segment.
     */
    struct encryption_info_command
    {
        /// LC_ENCRYPTION_INFO.
        uint cmd;

        /// Sizeof(struct encryption_info_command).
        uint cmdsize;

        /// File offset of encrypted range.
        uint cryptoff;

        /// File size of encrypted range.
        uint cryptsize;

        /// Which encryption system, 0 means not-encrypted yet.
        uint cryptid;
    }

    /**
     * The encryption_info_command_64 contains the file offset and size of an
     * of an encrypted segment (for use in x86_64 targets).
     */
    struct encryption_info_command_64
    {
        /// LC_ENCRYPTION_INFO_64.
        uint cmd;

        /// Sizeof(struct encryption_info_command_64).
        uint cmdsize;

        /// File offset of encrypted range.
        uint cryptoff;

        /// File size of encrypted range.
        uint cryptsize;

        /// Which encryption system, 0 means not-encrypted yet.
        uint cryptid;

        /// Padding to make this struct's size a multiple of 8 bytes.
        uint pad;
    }

    /**
     * The version_min_command contains the min OS version on which this
     * binary was built to run.
     */
    struct version_min_command
    {
        /**
         * LC_VERSION_MIN_MACOSX or LC_VERSION_MIN_IPHONEOS or
         * LC_VERSION_MIN_WATCHOS or LC_VERSION_MIN_TVOS.
         */
        uint cmd;

        /// Sizeof(struct min_version_command).
        uint cmdsize;

        /// X.Y.Z is encoded in nibbles xxxx.yy.zz.
        uint version_;

        /// X.Y.Z is encoded in nibbles xxxx.yy.zz.
        uint sdk;
    }

    /**
     * The build_version_command contains the min OS version on which this
     * binary was built to run for its platform. The list of known platforms and
     * tool values following it.
     */
    struct build_version_command
    {
        /// LC_BUILD_VERSION.
        uint cmd;

        /**
         * Sizeof(struct build_version_command) plus ntools
         * sizeof(struct build_tool_version).
         */
        uint cmdsize;

        /// Platform.
        uint platform;

        /// X.Y.Z is encoded in nibbles xxxx.yy.zz.
        uint minos;

        /// X.Y.Z is encoded in nibbles xxxx.yy.zz.
        uint sdk;

        /// Number of tool entries following this.
        uint ntools;

    }

    ///
    struct build_tool_version
    {
        /// Enum for the tool.
        uint tool;

        /// Version number of the tool.
        uint version_;
    }

    /// Known values for the platform field above.
    enum
    {
        ///
        PLATFORM_MACOS,

        ///
        PLATFORM_IOS,

        ///
        PLATFORM_TVOS,

        ///
        PLATFORM_WATCHOS,

        ///
        PLATFORM_BRIDGEOS,

        ///
        PLATFORM_UIKITFORMAC,

        ///
        PLATFORM_IOSSIMULATOR,

        ///
        PLATFORM_TVOSSIMULATOR,

        ///
        PLATFORM_WATCHOSSIMULATOR,

        ///
        PLATFORM_DRIVERKIT
    }


    /// Known values for the tool field above.
    enum
    {
        ///
        TOOL_CLANG,

        ///
        TOOL_SWIFT,

        ///
        TOOL_LD
    }

    /**
     * The dyld_info_command contains the file offsets and sizes of
     * the new compressed form of the information dyld needs to
     * load the image. This information is used by dyld on Mac OS X
     * 10.6 and later. All information pointed to by this command
     * is encoded using byte streams, so no endian swapping is needed
     * to interpret it.
     */
    struct dyld_info_command
    {
        /// LC_DYLD_INFO or LC_DYLD_INFO_ONLY.
        uint cmd;

        /// Sizeof(struct dyld_info_command).
        uint cmdsize;



        /**
         * File offset to rebase info.
         *
         * Dyld rebases an image whenever dyld loads it at an address different
         * from its preferred address. The rebase information is a stream
         * of byte sized opcodes whose symbolic names start with REBASE_OPCODE_.
         * Conceptually the rebase information is a table of tuples:
         *    <seg-index, seg-offset, type>
         * The opcodes are a compressed way to encode the table by only
         * encoding when a column changes. In addition simple patterns
         * like "every n'th offset for m times" can be encoded in a few
         * bytes.
         */
        uint rebase_off;

        /// Size of rebase info.
        uint rebase_size;



        /**
         * File offset to binding info.
         *
         * Dyld binds an image during the loading process, if the image
         * requires any pointers to be initialized to symbols in other images.
         * The bind information is a stream of byte sized
         * opcodes whose symbolic names start with BIND_OPCODE_.
         * Conceptually the bind information is a table of tuples:
         *    <seg-index, seg-offset, type, symbol-library-ordinal, symbol-name, addend>
         * The opcodes are a compressed way to encode the table by only
         * encoding when a column changes. In addition simple patterns
         * like for runs of pointers initialzed to the same value can be
         * encoded in a few bytes.
         */
        uint bind_off;

        /// Size of binding info.
        uint bind_size;



        /**
         * File offset to weak binding info.
         *
         * Some C++ programs require dyld to unique symbols so that all
         * images in the process use the same copy of some code/data.
         * This step is done after binding. The content of the weak_bind
         * info is an opcode stream like the bind_info. But it is sorted
         * alphabetically by symbol name. This enable dyld to walk
         * all images with weak binding information in order and look
         * for collisions. If there are no collisions, dyld does
         * no updating. That means that some fixups are also encoded
         * in the bind_info. For instance, all calls to "operator new"
         * are first bound to libstdc++.dylib using the information
         * in bind_info. Then if some image overrides operator new
         * that is detected when the weak_bind information is processed
         * and the call to operator new is then rebound.
         */
        uint weak_bind_off;

        /// Size of weak binding info.
        uint weak_bind_size;



        /**
         * File offset to lazy binding info.
         *
         * Some uses of external symbols do not need to be bound immediately.
         * Instead they can be lazily bound on first use. The lazy_bind
         * are contains a stream of BIND opcodes to bind all lazy symbols.
         * Normal use is that dyld ignores the lazy_bind section when
         * loading an image. Instead the static linker arranged for the
         * lazy pointer to initially point to a helper function which
         * pushes the offset into the lazy_bind area for the symbol
         * needing to be bound, then jumps to dyld which simply adds
         * the offset to lazy_bind_off to get the information on what
         * to bind.
         */
        uint lazy_bind_off;

        /// Size of lazy binding infs.
        uint lazy_bind_size;



        /**
         * File offset to lazy binding info.
         *
         * The symbols exported by a dylib are encoded in a trie. This
         * is a compact representation that factors out common prefixes.
         * It also reduces LINKEDIT pages in RAM because it encodes all
         * information (name, address, flags) in one small, contiguous range.
         * The export area is a stream of nodes. The first node sequentially
         * is the start node for the trie.
         *
         * Nodes for a symbol start with a uleb128 that is the length of
         * the exported symbol information for the string so far.
         * If there is no exported symbol, the node starts with a zero byte.
         * If there is exported info, it follows the length.
         *
         * First is a uleb128 containing flags. Normally, it is followed by
         * a uleb128 encoded offset which is location of the content named
         * by the symbol from the mach_header for the image. If the flags
         * is EXPORT_SYMBOL_FLAGS_REEXPORT, then following the flags is
         * a uleb128 encoded library ordinal, then a zero terminated
         * UTF8 string. If the string is zero length, then the symbol
         * is re-export from the specified dylib with the same name.
         * If the flags is EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER, then following
         * the flags is two uleb128s: the stub offset and the resolver offset.
         * The stub is used by non-lazy pointers. The resolver is used
         * by lazy pointers and must be called to get the actual address to use.
         *
         * After the optional exported symbol information is a byte of
         * how many edges (0-255) that this node has leaving it,
         * followed by each edge.
         * Each edge is a zero terminated UTF8 of the addition chars
         * in the symbol, followed by a uleb128 offset for the node that
         * edge points to.
         *
         */
        uint export_off;

        /// Size of lazy binding infs.
        uint export_size;
    }

    /// The following are used to encode rebasing information.
    enum
    {
        ///
        REBASE_TYPE_POINTER,

        ///
        REBASE_TYPE_TEXT_ABSOLUTE32,

        ///
        REBASE_TYPE_TEXT_PCREL32,



        ///
        REBASE_OPCODE_MASK,

        ///
        REBASE_IMMEDIATE_MASK,

        ///
        REBASE_OPCODE_DONE,

        ///
        REBASE_OPCODE_SET_TYPE_IMM,

        ///
        REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB,

        ///
        REBASE_OPCODE_ADD_ADDR_ULEB,

        ///
        REBASE_OPCODE_ADD_ADDR_IMM_SCALED,

        ///
        REBASE_OPCODE_DO_REBASE_IMM_TIMES,

        ///
        REBASE_OPCODE_DO_REBASE_ULEB_TIMES,

        ///
        REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB,

        ///
        REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB
    }


    /// The following are used to encode binding information.
    enum
    {
        ///
        BIND_TYPE_POINTER,

        ///
        BIND_TYPE_TEXT_ABSOLUTE32,

        ///
        BIND_TYPE_TEXT_PCREL32,



        ///
        BIND_SPECIAL_DYLIB_SELF,

        ///
        BIND_SPECIAL_DYLIB_MAIN_EXECUTABLE,

        ///
        BIND_SPECIAL_DYLIB_FLAT_LOOKUP,

        ///
        BIND_SPECIAL_DYLIB_WEAK_LOOKUP,



        ///
        BIND_SYMBOL_FLAGS_WEAK_IMPORT,

        ///
        BIND_SYMBOL_FLAGS_NON_WEAK_DEFINITION,



        ///
        BIND_OPCODE_MASK,

        ///
        BIND_IMMEDIATE_MASK,

        ///
        BIND_OPCODE_DONE,

        ///
        BIND_OPCODE_SET_DYLIB_ORDINAL_IMM,

        ///
        BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB,

        ///
        BIND_OPCODE_SET_DYLIB_SPECIAL_IMM,

        ///
        BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM,

        ///
        BIND_OPCODE_SET_TYPE_IMM,

        ///
        BIND_OPCODE_SET_ADDEND_SLEB,

        ///
        BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB,

        ///
        BIND_OPCODE_ADD_ADDR_ULEB,

        ///
        BIND_OPCODE_DO_BIND,

        ///
        BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB,

        ///
        BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED,

        ///
        BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB,

        ///
        BIND_OPCODE_THREADED,

        ///
        BIND_SUBOPCODE_THREADED_SET_BIND_ORDINAL_TABLE_SIZE_ULEB,

        ///
        BIND_SUBOPCODE_THREADED_APPLY
    }

    /**
     * The following are used on the flags byte of a terminal node
     * in the export information.
     */
    enum
    {

        ///
        EXPORT_SYMBOL_FLAGS_KIND_MASK,

        ///
        EXPORT_SYMBOL_FLAGS_KIND_REGULAR,

        ///
        EXPORT_SYMBOL_FLAGS_KIND_THREAD_LOCAL,

        ///
        EXPORT_SYMBOL_FLAGS_KIND_ABSOLUTE,

        ///
        EXPORT_SYMBOL_FLAGS_WEAK_DEFINITION,

        ///
        EXPORT_SYMBOL_FLAGS_REEXPORT,

        ///
        EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER,

    }


    /*
     * The linker_option_command contains linker options embedded in object files.
     */
    struct linker_option_command
    {
        /// LC_LINKER_OPTION only used in MH_OBJECT filetypes.
        uint cmd;

        ///
        uint cmdsize;

        /**
         * Number of strings concatenation of zero terminated UTF8 strings.
         * Zero filled at end to align.
         */
        uint count;
    }

    /**
     * The symseg_command contains the offset and size of the GNU style
     * symbol table information as described in the header file <symseg.h>.
     * The symbol roots of the symbol segments must also be aligned properly
     * in the file. So the requirement of keeping the offsets aligned to a
     * multiple of a 4 bytes translates to the length field of the symbol
     * roots also being a multiple of a long. Also the padding must again be
     * zeroed. (THIS IS OBSOLETE and no longer supported).
     */
    struct symseg_command
    {
        /// LC_SYMSEG.
        uint cmd;

        /// Sizeof(struct symseg_command).
        uint cmdsize;

        /// Symbol segment offset.
        uint offset;

        /// Symbol segment size in bytes.
        uint size;
    }

    /**
     * The ident_command contains a free format string table following the
     * ident_command structure. The strings are null terminated and the size of
     * the command is padded out with zero bytes to a multiple of 4 bytes/
     * (THIS IS OBSOLETE and no longer supported).
     */
    struct ident_command
    {
        /// LC_IDENT.
        uint cmd;

        /// Strings that follow this command.
        uint cmdsize;
    }

    /**
     * The fvmfile_command contains a reference to a file to be loaded at the
     * specified virtual address. (Presently, this command is reserved for
     * internal use. The kernel ignores this command when loading a program into
     * memory).
     */
    struct fvmfile_command
    {
        /// LC_FVMFILE.
        uint cmd;

        /// Includes pathname string.
        uint cmdsize;

        /// Files pathname.
        lc_str name;

        /// Files virtual address.
        uint header_addr;
    }

    /**
     * The entry_point_command is a replacement for thread_command.
     * It is used for main executables to specify the location (file offset)
     * of main(). If -stack_size was used at link time, the stacksize
     * field will contain the stack size need for the main thread.
     */
    struct entry_point_command
    {
        /// LC_MAIN only used in MH_EXECUTE filetypes.
        uint cmd;

        /// 24.
        uint cmdsize;

        /// File (__TEXT) offset of main().
        ulong entryoff;

        /// If not zero, initial stack size.
        ulong stacksize;
    }

    /**
     * The source_version_command is an optional load command containing
     * the version of the sources used to build the binary.
     */
    struct source_version_command
    {
        /// LC_SOURCE_VERSION.
        uint cmd;

        /// 16.
        uint cmdsize;

        /// A.B.C.D.E packed as a24.b10.c10.d10.e10.
        ulong version_;
    }

    /**
     * The LC_DATA_IN_CODE load commands uses a linkedit_data_command
     * to point to an array of data_in_code_entry entries. Each entry
     * describes a range of data in a code section.
     */
    struct data_in_code_entry
    {
        /// From mach_header to start of data range.
        uint offset;

        /// Number of bytes in data range.
        ushort length;

        /// A DICE_KIND_* value.
        ushort kind;
    }

    ///
    enum
    {
        ///
        DICE_KIND_DATA,

        ///
        DICE_KIND_JUMP_TABLE8,

        ///
        DICE_KIND_JUMP_TABLE16,

        ///
        DICE_KIND_JUMP_TABLE32,

        ///
        DICE_KIND_ABS_JUMP_TABLE32
    }

    /**
     * Sections of type S_THREAD_LOCAL_VARIABLES contain an array
     * of tlv_descriptor structures.
     */
    struct tlv_descriptor
    {
        ///
        void* function (tlv_descriptor*) thunk;

        ///
        c_ulong key;

        ///
        c_ulong offset;
    }

    /**
     * LC_NOTE commands describe a region of arbitrary data included in a Mach-O
     * file. Its initial use is to record extra data in MH_CORE files.
     */
    struct note_command
    {
        /// LC_NOTE.
        uint cmd;

        /// Sizeof(struct note_command).
        uint cmdsize;

        /// Owner name for this LC_NOTE.
        char[16] data_owner;

        /// File offset of this data.
        ulong offset;

        /// Length of data region.
        ulong size;
    }
}
else
    version = Darwin;

version (Darwin):
extern (C):

struct mach_header
{
    uint magic;
    int cputype;
    int cpusubtype;
    uint filetype;
    uint ncmds;
    uint sizeofcmds;
    uint flags;
}

enum
{
    MH_MAGIC = 0xfeedface,
    MH_CIGAM = 0xcefaedfe
}

struct mach_header_64
{
    uint magic;
    int cputype;
    int cpusubtype;
    uint filetype;
    uint ncmds;
    uint sizeofcmds;
    uint flags;
    uint reserved;
}

enum
{
    MH_MAGIC_64 = 0xfeedfacf,
    MH_CIGAM_64 = 0xcffaedfe
}

enum
{
    MH_OBJECT = 0x1,
    MH_EXECUTE = 0x2,
    MH_FVMLIB = 0x3,
    MH_CORE = 0x4,
    MH_PRELOAD = 0x5,
    MH_DYLIB = 0x6,
    MH_DYLINKER = 0x7,
    MH_BUNDLE = 0x8,
    MH_DYLIB_STUB = 0x9,
    MH_DSYM = 0xa,
    MH_KEXT_BUNDLE = 0xb
}

enum
{
    MH_NOUNDEFS = 0x1,
    MH_INCRLINK = 0x2,
    MH_DYLDLINK = 0x4,
    MH_BINDATLOAD = 0x8,
    MH_PREBOUND = 0x10,
    MH_SPLIT_SEGS = 0x20,
    MH_LAZY_INIT = 0x40,
    MH_TWOLEVEL = 0x80,
    MH_FORCE_FLAT = 0x100,
    MH_NOMULTIDEFS = 0x200,
    MH_NOFIXPREBINDING = 0x400,
    MH_PREBINDABLE = 0x800,
    MH_ALLMODSBOUND = 0x1000,
    MH_SUBSECTIONS_VIA_SYMBOLS = 0x2000,
    MH_CANONICAL = 0x4000,
    MH_WEAK_DEFINES = 0x8000,
    MH_BINDS_TO_WEAK = 0x10000,

    MH_ALLOW_STACK_EXECUTION = 0x20000,
    MH_ROOT_SAFE = 0x40000,

    MH_SETUID_SAFE = 0x80000,
    MH_NO_REEXPORTED_DYLIBS = 0x100000,
    MH_PIE = 0x200000,
    MH_DEAD_STRIPPABLE_DYLIB = 0x400000,
    MH_HAS_TLV_DESCRIPTORS = 0x800000,

    MH_NO_HEAP_EXECUTION = 0x1000000,

    MH_APP_EXTENSION_SAFE = 0x02000000,

    MH_NLIST_OUTOFSYNC_WITH_DYLDINFO = 0x04000000,

    MH_SIM_SUPPORT = 0x08000000,

    MH_DYLIB_IN_CACHE = 0x80000000
}

struct load_command
{
    uint cmd;
    uint cmdsize;
}

enum LC_REQ_DYLD = 0x80000000;

enum
{
    LC_SEGMENT = 0x1,
    LC_SYMTAB = 0x2,
    LC_SYMSEG = 0x3,
    LC_THREAD = 0x4,
    LC_UNIXTHREAD = 0x5,
    LC_LOADFVMLIB = 0x6,
    LC_IDFVMLIB = 0x7,
    LC_IDENT = 0x8,
    LC_FVMFILE = 0x9,
    LC_PREPAGE = 0xa,
    LC_DYSYMTAB = 0xb,
    LC_LOAD_DYLIB = 0xc,
    LC_ID_DYLIB = 0xd,
    LC_LOAD_DYLINKER = 0xe,
    LC_ID_DYLINKER = 0xf,
    LC_PREBOUND_DYLIB = 0x10,
    LC_ROUTINES = 0x11,
    LC_SUB_FRAMEWORK = 0x12,
    LC_SUB_UMBRELLA = 0x13,
    LC_SUB_CLIENT = 0x14,
    LC_SUB_LIBRARY = 0x15,
    LC_TWOLEVEL_HINTS = 0x16,
    LC_PREBIND_CKSUM = 0x17
}

enum LC_LOAD_WEAK_DYLIB = 0x18 | LC_REQ_DYLD;

enum
{
    LC_SEGMENT_64 = 0x19,
    LC_ROUTINES_64 = 0x1a,
    LC_UUID = 0x1b,
    LC_RPATH = 0x1c | LC_REQ_DYLD,
    LC_CODE_SIGNATURE = 0x1d,
    LC_SEGMENT_SPLIT_INFO = 0x1e,
    LC_REEXPORT_DYLIB = 0x1f | LC_REQ_DYLD,
    LC_LAZY_LOAD_DYLIB = 0x20,
    LC_ENCRYPTION_INFO = 0x21,
    LC_DYLD_INFO = 0x22,
    LC_DYLD_INFO_ONLY = 0x22 | LC_REQ_DYLD,
    LC_LOAD_UPWARD_DYLIB = 0x23 | LC_REQ_DYLD,
    LC_VERSION_MIN_MACOSX = 0x24,
    LC_VERSION_MIN_IPHONEOS = 0x25,
    LC_FUNCTION_STARTS = 0x26,
    LC_DYLD_ENVIRONMENT = 0x27,
    LC_MAIN = 0x28 | LC_REQ_DYLD,
    LC_DATA_IN_CODE = 0x29,
    LC_SOURCE_VERSION = 0x2A,
    LC_DYLIB_CODE_SIGN_DRS = 0x2B,
    LC_ENCRYPTION_INFO_64 = 0x2C,
    LC_LINKER_OPTION = 0x2D,
    LC_LINKER_OPTIMIZATION_HINT = 0x2E,
    LC_VERSION_MIN_TVOS = 0x2F,
    LC_VERSION_MIN_WATCHOS = 0x30,
    LC_NOTE = 0x31,
    LC_BUILD_VERSION = 0x32,
    LC_DYLD_EXPORTS_TRIE = 0x33 | LC_REQ_DYLD,
    LC_DYLD_CHAINED_FIXUPS = 0x34 | LC_REQ_DYLD
}

union lc_str
{
    uint offset;

    version (D_LP64) {}
    else
        char* ptr;
}

struct segment_command
{
    uint cmd;
    uint cmdsize;
    char[16] segname = 0;
    uint vmaddr;
    uint vmsize;
    uint fileoff;
    uint filesize;
    int maxprot;
    int initprot;
    uint nsects;
    uint flags;
}

struct segment_command_64
{
    uint cmd;
    uint cmdsize;
    char[16] segname = 0;
    ulong vmaddr;
    ulong vmsize;
    ulong fileoff;
    ulong filesize;
    int maxprot;
    int initprot;
    uint nsects;
    uint flags;
}

enum
{
    SG_HIGHVM = 0x1,
    SG_FVMLIB = 0x2,
    SG_NORELOC = 0x4,
    SG_PROTECTED_VERSION_1 = 0x8,
    SG_READ_ONLY = 0x10
}

struct section
{
    char[16] sectname = 0;
    char[16] segname = 0;
    uint addr;
    uint size;
    uint offset;
    uint align_;
    uint reloff;
    uint nreloc;
    uint flags;
    uint reserved1;
    uint reserved2;
}

struct section_64
{
    char[16] sectname = 0;
    char[16] segname = 0;
    ulong addr;
    ulong size;
    uint offset;
    uint align_;
    uint reloff;
    uint nreloc;
    uint flags;
    uint reserved1;
    uint reserved2;
    uint reserved3;
}


enum
{
    SECTION_TYPE = 0x000000ff,
    SECTION_ATTRIBUTES = 0xffffff00
}

enum
{
    S_REGULAR = 0x0,
    S_ZEROFILL = 0x1,
    S_CSTRING_LITERALS = 0x2,
    S_4BYTE_LITERALS = 0x3,
    S_8BYTE_LITERALS = 0x4,
    S_LITERAL_POINTERS = 0x5,
    S_NON_LAZY_SYMBOL_POINTERS = 0x6,
    S_LAZY_SYMBOL_POINTERS = 0x7,
    S_SYMBOL_STUBS = 0x8,
    S_MOD_INIT_FUNC_POINTERS = 0x9,
    S_MOD_TERM_FUNC_POINTERS = 0xa,
    S_COALESCED = 0xb,
    S_GB_ZEROFILL = 0xc,
    S_INTERPOSING = 0xd,
    S_16BYTE_LITERALS = 0xe,
    S_DTRACE_DOF = 0xf,
    S_LAZY_DYLIB_SYMBOL_POINTERS = 0x10,

    S_THREAD_LOCAL_REGULAR = 0x11,
    S_THREAD_LOCAL_ZEROFILL = 0x12,
    S_THREAD_LOCAL_VARIABLES = 0x13,
    S_THREAD_LOCAL_VARIABLE_POINTERS = 0x14,
    S_THREAD_LOCAL_INIT_FUNCTION_POINTERS = 0x15,
    S_INIT_FUNC_OFFSETS = 0x16
}

enum
{
    SECTION_ATTRIBUTES_USR = 0xff000000,
    S_ATTR_PURE_INSTRUCTIONS = 0x80000000,
    S_ATTR_NO_TOC = 0x40000000,
    S_ATTR_STRIP_STATIC_SYMS = 0x20000000,
    S_ATTR_NO_DEAD_STRIP = 0x10000000,
    S_ATTR_LIVE_SUPPORT = 0x08000000,
    S_ATTR_SELF_MODIFYING_CODE = 0x04000000,
    S_ATTR_DEBUG = 0x02000000,
    SECTION_ATTRIBUTES_SYS = 0x00ffff00,
    S_ATTR_SOME_INSTRUCTIONS = 0x00000400,
    S_ATTR_EXT_RELOC = 0x00000200,
    S_ATTR_LOC_RELOC = 0x00000100
}

enum
{
    SEG_PAGEZERO = "__PAGEZERO",

    SEG_TEXT = "__TEXT",
    SECT_TEXT = "__text",
    SECT_FVMLIB_INIT0 = "__fvmlib_init0",
    SECT_FVMLIB_INIT1 = "__fvmlib_init1",

    SEG_DATA = "__DATA",
    SECT_DATA = "__data",
    SECT_BSS = "__bss",
    SECT_COMMON = "__common",

    SEG_OBJC = "__OBJC",
    SECT_OBJC_SYMBOLS = "__symbol_table",
    SECT_OBJC_MODULES = "__module_info",
    SECT_OBJC_STRINGS = "__selector_strs",
    SECT_OBJC_REFS = "__selector_refs",

    SEG_ICON = "__ICON",
    SECT_ICON_HEADER = "__header",
    SECT_ICON_TIFF = "__tiff",

    SEG_LINKEDIT = "__LINKEDIT",

    SEG_UNIXSTACK = "__UNIXSTACK",

    SEG_IMPORT = "__IMPORT"
}

struct fvmlib
{
    lc_str name;
    uint minor_version;
    uint header_addr;
}

struct fvmlib_command
{
    uint cmd;
    uint cmdsize;
    fvmlib fvmlib_;
}

struct dylib
{
    lc_str name;
    uint timestamp;
    uint current_version;
    uint compatibility_version;
}

struct dylib_command
{
    uint cmd;
    uint cmdsize;
    dylib dylib_;
}

struct sub_framework_command
{
    uint cmd;
    uint cmdsize;
    lc_str umbrella;
}

struct sub_client_command
{
    uint cmd;
    uint cmdsize;
    lc_str client;
}

struct sub_umbrella_command
{
    uint cmd;
    uint cmdsize;
    lc_str sub_umbrella;
}

struct sub_library_command
{
    uint cmd;
    uint cmdsize;
    lc_str sub_library;
}

struct prebound_dylib_command
{
    uint cmd;
    uint cmdsize;
    lc_str name;
    uint nmodules;
    lc_str linked_modules;
}

struct dylinker_command
{
    uint cmd;
    uint cmdsize;
    lc_str name;
}

struct thread_command
{
    uint cmd;
    uint cmdsize;
}

struct routines_command
{
    uint cmd;
    uint cmdsize;
    uint init_address;
    uint init_module;
    uint reserved1;
    uint reserved2;
    uint reserved3;
    uint reserved4;
    uint reserved5;
    uint reserved6;
}

struct routines_command_64
{
    uint cmd;
    uint cmdsize;
    ulong init_address;
    ulong init_module;
    ulong reserved1;
    ulong reserved2;
    ulong reserved3;
    ulong reserved4;
    ulong reserved5;
    ulong reserved6;
}

struct symtab_command
{
    uint cmd;
    uint cmdsize;
    uint symoff;
    uint nsyms;
    uint stroff;
    uint strsize;
}

struct dysymtab_command
{
    uint cmd;
    uint cmdsize;

    uint ilocalsym;
    uint nlocalsym;

    uint iextdefsym;
    uint nextdefsym;

    uint iundefsym;
    uint nundefsym;

    uint tocoff;
    uint ntoc;

    uint modtaboff;
    uint nmodtab;

    uint extrefsymoff;
    uint nextrefsyms;

    uint indirectsymoff;
    uint nindirectsyms;

    uint extreloff;
    uint nextrel;

    uint locreloff;
    uint nlocrel;
}

enum
{
    INDIRECT_SYMBOL_LOCAL = 0x80000000,
    INDIRECT_SYMBOL_ABS = 0x40000000
}

struct dylib_table_of_contents
{
    uint symbol_index;
    uint module_index;
}

struct dylib_module
{
    uint module_name;

    uint iextdefsym;
    uint nextdefsym;
    uint irefsym;
    uint nrefsym;
    uint ilocalsym;
    uint nlocalsym;

    uint iextrel;
    uint nextrel;

    uint iinit_iterm;
    uint ninit_nterm;

    uint objc_module_info_addr;
    uint objc_module_info_size;
}

struct dylib_module_64
{
    uint module_name;

    uint iextdefsym;
    uint nextdefsym;
    uint irefsym;
    uint nrefsym;
    uint ilocalsym;
    uint nlocalsym;

    uint iextrel;
    uint nextrel;

    uint iinit_iterm;
    uint ninit_nterm;

    uint objc_module_info_size;
    ulong objc_module_info_addr;
}

struct dylib_reference
{
    private uint storage;

    @property uint isym()() const pure nothrow @nogc @safe
    {
        return cast(uint) ((storage & 16777215U) >> 0U);
    }

    @property void isym()(uint v) @safe pure nothrow @nogc
    in(v >= 0U, "Value is smaller than the minimum value of bitfield 'isym'")
    in(v <= 16777215U, "Value is greater than the maximum value of bitfield 'isym'")
    {
        storage = cast(uint) ((storage & (-1 - cast(uint) 16777215U)) |
            ((cast(uint) v << 0U) & 16777215U));
    }

    @property uint flags()() const pure nothrow @nogc @safe
    {
        return cast(uint) ((storage & 4278190080U) >> 24U);
    }

    @property void flags()(uint v) pure nothrow @nogc @safe
    in(v >= 0U, "Value is smaller than the minimum value of bitfield 'flags'")
    in(v <= 255U, "Value is greater than the maximum value of bitfield 'flags'")
    {
        storage = cast(uint) ((storage & (-1 - cast(uint) 4278190080U)) |
            ((cast(uint) v << 24U) & 4278190080U));
    }
}

struct twolevel_hints_command
{
    uint cmd;
    uint cmdsize;
    uint offset;
    uint nhints;
}

struct twolevel_hint
{
    private uint storage;

    @property uint isub_image()() const pure nothrow @nogc @safe
    {
        return cast(uint) ((storage & 255U) >>0U);
    }

    @property void isub_image()(uint v) pure nothrow @nogc @safe
    in(v >= 0U, "Value is smaller than the minimum value of bitfield 'isub_image'")
    in(v <= 255U, "Value is greater than the maximum value of bitfield 'isub_image'")
    {
        storage = cast(uint) ((storage & (-1-cast(uint)255U)) |
            ((cast(uint) v << 0U) & 255U));
    }

    @property uint itoc()() const pure nothrow @nogc @safe
    {
        return cast(uint) ((storage & 4294967040U) >>8U);
    }

    @property void itoc()(uint v) pure nothrow @nogc @safe
    in(v >= 0U, "Value is smaller than the minimum value of bitfield 'itoc'")
    in(v <= 16777215U, "Value is greater than the maximum value of bitfield 'itoc'")
    {
        storage = cast(uint) ((storage & (-1-cast(uint)4294967040U)) |
            ((cast(uint) v << 8U) & 4294967040U));
    }
}

struct prebind_cksum_command
{
    uint cmd;
    uint cmdsize;
    uint cksum;
}

struct uuid_command
{
    uint cmd;
    uint cmdsize;
    ubyte[16] uuid;
}

struct rpath_command
{
    uint cmd;
    uint cmdsize;
    lc_str path;
}

struct linkedit_data_command
{
    uint cmd;
    uint cmdsize;
    uint dataoff;
    uint datasize;
}

struct encryption_info_command
{
    uint cmd;
    uint cmdsize;
    uint cryptoff;
    uint cryptsize;
    uint cryptid;
}

struct encryption_info_command_64
{
    uint cmd;
    uint cmdsize;
    uint cryptoff;
    uint cryptsize;
    uint cryptid;
    uint pad;
}

struct version_min_command
{
    uint cmd;
    uint cmdsize;
    uint version_;
    uint sdk;
}

struct build_version_command
{
    uint cmd;
    uint cmdsize;

    uint platform;
    uint minos;
    uint sdk;
    uint ntools;
}

struct build_tool_version
{
    uint tool;
    uint version_;
}

enum
{
    PLATFORM_MACOS = 1,
    PLATFORM_IOS = 2,
    PLATFORM_TVOS = 3,
    PLATFORM_WATCHOS = 4,
    PLATFORM_BRIDGEOS = 5,
    PLATFORM_UIKITFORMAC = 6,
    PLATFORM_IOSSIMULATOR = 7,
    PLATFORM_TVOSSIMULATOR = 8,
    PLATFORM_WATCHOSSIMULATOR = 9,
    PLATFORM_DRIVERKIT = 10
}

enum
{
    TOOL_CLANG = 1,
    TOOL_SWIFT = 2,
    TOOL_LD = 3
}

struct dyld_info_command
{
    uint cmd;
    uint cmdsize;

    uint rebase_off;
    uint rebase_size;

    uint bind_off;
    uint bind_size;

    uint weak_bind_off;
    uint weak_bind_size;

    uint lazy_bind_off;
    uint lazy_bind_size;

    uint export_off;
    uint export_size;
}

enum
{
    REBASE_TYPE_POINTER = 1,
    REBASE_TYPE_TEXT_ABSOLUTE32 = 2,
    REBASE_TYPE_TEXT_PCREL32 = 3,

    REBASE_OPCODE_MASK = 0xF0,
    REBASE_IMMEDIATE_MASK = 0x0F,
    REBASE_OPCODE_DONE = 0x00,
    REBASE_OPCODE_SET_TYPE_IMM = 0x10,
    REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB = 0x20,
    REBASE_OPCODE_ADD_ADDR_ULEB = 0x30,
    REBASE_OPCODE_ADD_ADDR_IMM_SCALED = 0x40,
    REBASE_OPCODE_DO_REBASE_IMM_TIMES = 0x50,
    REBASE_OPCODE_DO_REBASE_ULEB_TIMES = 0x60,
    REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB = 0x70,
    REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB = 0x80
}

enum
{
    BIND_TYPE_POINTER = 1,
    BIND_TYPE_TEXT_ABSOLUTE32 = 2,
    BIND_TYPE_TEXT_PCREL32 = 3,

    BIND_SPECIAL_DYLIB_SELF = 0,
    BIND_SPECIAL_DYLIB_MAIN_EXECUTABLE = -1,
    BIND_SPECIAL_DYLIB_FLAT_LOOKUP = -2,
    BIND_SPECIAL_DYLIB_WEAK_LOOKUP = -3,

    BIND_SYMBOL_FLAGS_WEAK_IMPORT = 0x1,
    BIND_SYMBOL_FLAGS_NON_WEAK_DEFINITION = 0x8,

    BIND_OPCODE_MASK = 0xF0,
    BIND_IMMEDIATE_MASK = 0x0F,
    BIND_OPCODE_DONE = 0x00,
    BIND_OPCODE_SET_DYLIB_ORDINAL_IMM = 0x10,
    BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB = 0x20,
    BIND_OPCODE_SET_DYLIB_SPECIAL_IMM = 0x30,
    BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM = 0x40,
    BIND_OPCODE_SET_TYPE_IMM = 0x50,
    BIND_OPCODE_SET_ADDEND_SLEB = 0x60,
    BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB = 0x70,
    BIND_OPCODE_ADD_ADDR_ULEB = 0x80,
    BIND_OPCODE_DO_BIND = 0x90,
    BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB = 0xA0,
    BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED = 0xB0,
    BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB = 0xC0,
    BIND_OPCODE_THREADED = 0xD0,
    BIND_SUBOPCODE_THREADED_SET_BIND_ORDINAL_TABLE_SIZE_ULEB = 0x00,
    BIND_SUBOPCODE_THREADED_APPLY = 0x01
}

enum
{
    EXPORT_SYMBOL_FLAGS_KIND_MASK = 0x03,
    EXPORT_SYMBOL_FLAGS_KIND_REGULAR = 0x00,
    EXPORT_SYMBOL_FLAGS_KIND_THREAD_LOCAL = 0x01,
    EXPORT_SYMBOL_FLAGS_KIND_ABSOLUTE = 0x02,
    EXPORT_SYMBOL_FLAGS_WEAK_DEFINITION = 0x04,
    EXPORT_SYMBOL_FLAGS_REEXPORT = 0x08,
    EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER = 0x10
}

struct linker_option_command
{
    uint cmd;
    uint cmdsize;
    uint count;
}

struct symseg_command
{
    uint cmd;
    uint cmdsize;
    uint offset;
    uint size;
}

struct ident_command
{
    uint cmd;
    uint cmdsize;
}

struct fvmfile_command
{
    uint cmd;
    uint cmdsize;
    lc_str name;
    uint header_addr;
}

struct entry_point_command
{
    uint cmd;
    uint cmdsize;
    ulong entryoff;
    ulong stacksize;
}

struct source_version_command
{
    uint cmd;
    uint cmdsize;
    ulong version_;
}

struct data_in_code_entry
{
    uint offset;
    ushort length;
    ushort kind;
}

enum
{
    DICE_KIND_DATA = 0x0001,
    DICE_KIND_JUMP_TABLE8 = 0x0002,
    DICE_KIND_JUMP_TABLE16 = 0x0003,
    DICE_KIND_JUMP_TABLE32 = 0x0004,
    DICE_KIND_ABS_JUMP_TABLE32 = 0x0005,
}

struct tlv_descriptor
{
    void* function(tlv_descriptor*) thunk;
    c_ulong key;
    c_ulong offset;
}

struct note_command
{
    uint cmd;
    uint cmdsize;
    char[16] data_owner = 0;
    ulong offset;
    ulong size;
}
