/**
 * Copyright: Copyright Digital Mars 2010-2018.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Jacob Carlborg
 * Version: Initial created: Feb 20, 2010-2018
 * Source: $(DRUNTIMESRC core/sys/darwin/mach/_loade.d)
 */
module core.sys.darwin.mach.loader;

version (CoreDdoc)
{
    /// Represents the header of a Mach-O file for 32-bit architecture.
    struct mach_header
    {
        /// The mach magic number identifier.
        uint magic;

        /// CPU specifier.
        int cputype;

        /// Machine specifier.
        int cpusubtype;

        /// The type of the file.
        uint filetype;

        /// Number of load commands.
        uint ncmds;

        /// The size of all the load commands.
        uint sizeofcmds;

        /// Flags.
        uint flags;
    }

    /// Represents the header of a Mach-O file for 64-bit architecture.
    struct mach_header_64
    {
        /// The mach magic number identifier.
        uint magic;

        /// CPU specifier.
        int cputype;

        /// Machine specifier.
        int cpusubtype;

        /// The type of the file.
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

    ///
    enum MH_MAGIC : uint;

    ///
    enum MH_CIGAM : uint;

    ///
    enum MH_MAGIC_64 : uint;

    ///
    enum MH_CIGAM_64 : uint;

    ///
    enum SEG_PAGEZERO : string;

    ///
    enum SEG_TEXT : string;

    ///
    enum SECT_TEXT : string;

    ///
    enum SECT_FVMLIB_INIT0 : string;

    ///
    enum SECT_FVMLIB_INIT1 : string;

    ///
    enum SEG_DATA : string;

    ///
    enum SECT_DATA : string;

    ///
    enum SECT_BSS : string;

    ///
    enum SECT_COMMON : string;

    ///
    enum SEG_OBJC : string;

    ///
    enum SECT_OBJC_SYMBOLS : string;

    ///
    enum SECT_OBJC_MODULES : string;

    ///
    enum SECT_OBJC_STRINGS : string;

    ///
    enum SECT_OBJC_REFS : string;

    ///
    enum SEG_ICON : string;

    ///
    enum SECT_ICON_HEADER : string;

    ///
    enum SECT_ICON_TIFF : string;

    ///
    enum SEG_LINKEDIT : string;

    ///
    enum SEG_UNIXSTACK : string;

    ///
    enum SEG_IMPORT : string;

    /// Represents a segment command in a Mach-O file for 32-bit architecture.
    struct segment_command
    {
        /// Type of load command, i.e. `LC_SEGMENT`.
        uint cmd;

        /// The size of this segment, includes size of section structs.
        uint cmdsize;

        /// The name of this segment.
        char[16] segname = 0;

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

        /// Number of sections in this segment.
        uint nsects;

        /// Flags.
        uint flags;
    }

    /// Represents a segment command in a Mach-O file for 64-bit architecture.
    struct segment_command_64
    {
        /// Type of load command, i.e. `LC_SEGMENT`.
        uint cmd;

        /// The size of this segment, includes size of section structs.
        uint cmdsize;

        /// The name of this segment.
        char[16] segname = 0;

        /// Memory address of this segment.
        long vmaddr;

        /// Memory size of this segment.
        long vmsize;

        /// File offset of this segment.
        long fileoff;

        /// Amount to map from the file.
        long filesize;

        /// Maximum VM protection.
        int maxprot;

        /// Initial VM protection.
        int initprot;

        /// Number of sections in this segment.
        uint nsects;

        /// Flags.
        uint flags;
    }

    /// Represents a section in a Mach-O file for 32-bit architecture.
    struct section
    {
        /// The name of this this section.
        char[16] sectname = 0;

        /// The name of the segment this section belongs to.
        char[16] segname = 0;

        /// The memory address of this section.
        uint addr;

        /// The size of this section in bytes.
        uint size;

        /// The file offset of this section.
        uint offset;

        /// The alignment (power of two) of this section.
        uint align_;

        /// The file offset of the relocation entries.
        uint reloff;

        /// The number of relocation entries.
        uint nreloc;

        /// Flags, section type and attributes.
        uint flags;

        /// Reserved.
        uint reserved1;

        /// Reserved.
        uint reserved2;
    }

    /// Represents a section in a Mach-O file for 64-bit architecture.
    struct section_64
    {
        /// The name of this this section.
        char[16] sectname = 0;

        /// The name of the segment this section belongs to.
        char[16] segname = 0;

        /// The memory address of this section.
        ulong addr;

        /// The size of this section in bytes.
        ulong size;

        /// The file offset of this section.
        uint offset;

        /// The alignment (power of two) of this section.
        uint align_;

        /// The file offset of the relocation entries.
        uint reloff;

        /// The number of relocation entries.
        uint nreloc;

        /// Flags, section type and attributes.
        uint flags;

        /// Reserved.
        uint reserved1;

        /// Reserved.
        uint reserved2;

        /// Reserved.
        uint reserved3;
    }
}

else version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
extern (C):

struct mach_header
{
    uint magic;
    int  cputype;
    int  cpusubtype;
    uint filetype;
    uint ncmds;
    uint sizeofcmds;
    uint flags;
}

struct mach_header_64
{
    uint magic;
    int  cputype;
    int  cpusubtype;
    uint filetype;
    uint ncmds;
    uint sizeofcmds;
    uint flags;
    uint reserved;
}

enum uint MH_MAGIC      = 0xfeedface;
enum uint MH_CIGAM      = 0xcefaedfe;
enum uint MH_MAGIC_64   = 0xfeedfacf;
enum uint MH_CIGAM_64   = 0xcffaedfe;

enum SEG_PAGEZERO       = "__PAGEZERO";
enum SEG_TEXT           = "__TEXT";
enum SECT_TEXT          = "__text";
enum SECT_FVMLIB_INIT0  = "__fvmlib_init0";
enum SECT_FVMLIB_INIT1  = "__fvmlib_init1";
enum SEG_DATA           = "__DATA";
enum SECT_DATA          = "__data";
enum SECT_BSS           = "__bss";
enum SECT_COMMON        = "__common";
enum SEG_OBJC           = "__OBJC";
enum SECT_OBJC_SYMBOLS  = "__symbol_table";
enum SECT_OBJC_MODULES  = "__module_info";
enum SECT_OBJC_STRINGS  = "__selector_strs";
enum SECT_OBJC_REFS     = "__selector_refs";
enum SEG_ICON           = "__ICON";
enum SECT_ICON_HEADER   = "__header";
enum SECT_ICON_TIFF     = "__tiff";
enum SEG_LINKEDIT       = "__LINKEDIT";
enum SEG_UNIXSTACK      = "__UNIXSTACK";
enum SEG_IMPORT         = "__IMPORT";

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
    long vmaddr;
    long vmsize;
    long fileoff;
    long filesize;
    int maxprot;
    int initprot;
    uint nsects;
    uint flags;
}

struct section
{
    char[16] sectname = 0;
    char[16] segname = 0;
    uint     addr;
    uint     size;
    uint     offset;
    uint     align_;
    uint     reloff;
    uint     nreloc;
    uint     flags;
    uint     reserved1;
    uint     reserved2;
}

struct section_64
{
    char[16] sectname = 0;
    char[16] segname = 0;
    ulong    addr;
    ulong    size;
    uint     offset;
    uint     align_;
    uint     reloff;
    uint     nreloc;
    uint     flags;
    uint     reserved1;
    uint     reserved2;
    uint     reserved3;
}

