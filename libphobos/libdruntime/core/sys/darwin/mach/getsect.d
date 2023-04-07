/**
 * D header file for $(LINK2 https://opensource.apple.com/source/cctools/cctools-895/include/mach-o/getsect.h.auto.html, mach-o/getsect.h).
 *
 * Copyright: Copyright Digital Mars 2010-2018.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Jacob Carlborg
 * Version: Initial created: Mar 16, 2010
 * Source: $(DRUNTIMESRC core/sys/darwin/mach/_getsect.d)
 */
module core.sys.darwin.mach.getsect;

extern (C):
nothrow:
@nogc:

version (CoreDdoc)
{
    import core.stdc.config : c_ulong;

    /**
     * In reality this will be $(REF mach_header, core, sys, darwin, mach, loader)
     * on 32-bit platforms and $(REF mach_header_64, core, sys, darwin, mach, loader)
     * 64-bit platforms.
     */
    struct MachHeader;

    /**
     * In reality this will be $(REF segment_command, core, sys, darwin, mach, loader)
     * on 32-bit platforms and $(REF segment_command_64, core, sys, darwin, mach, loader)
     * 64-bit platforms.
     */
    struct SegmentCommand;

    /**
     * In reality this will be $(REF section, core, sys, darwin, mach, loader)
     * on 32-bit platforms and $(REF section_64, core, sys, darwin, mach, loader)
     * 64-bit platforms.
     */
    struct Section;

    /**
     * Returns the section data of section with the given section name.
     *
     * Returns the section data of the given section in the given segment in the
     * mach executable it is linked into.
     *
     * ---
     * void main()
     * {
     *      import core.sys.darwin.mach.getsect;
     *      int size;
     *      assert(getsectdata("__TEXT", "__text", &size));
     *      assert(size > 0);
     * }
     * ---
     *
     * Params:
     *  segname = the name of the segment
     *  sectname = the name of the section
     *  size = this will be set to the size of the section or 0 if the section
     *      doesn't exist
     *
     * Returns: a pointer to the section data or `null` if it doesn't exist
     */
    char* getsectdata(
        const scope char* segname,
        const scope char* sectname,
        c_ulong *size
    );

    /**
     * Returns the section data of section with the given section name.
     *
     * Returns the section data of the given section in the given segment in the
     * given framework.
     *
     * ---
     * void main()
     * {
     *      import core.sys.darwin.mach.getsect;
     *      int size;
     *      assert(getsectdatafromFramework("Foundation", "__TEXT", "__text", &size));
     *      assert(size > 0);
     * }
     * ---
     *
     * Params:
     *  FrameworkName = the name of the framework to get the section data from
     *  segname = the name of the segment
     *  sectname = the name of the section
     *  size = this will be set to the size of the section or 0 if the section
     *      doesn't exist
     *
     * Returns: a pointer to the section data or `null` if it doesn't exist
     */
    char* getsectdatafromFramework(
        const scope char* FrameworkName,
        const scope char* segname,
        const scope char* sectname,
        c_ulong* size
    );

    ///
    c_ulong get_end();

    ///
    c_ulong get_etext();

    ///
    c_ulong get_edata();

    /**
     * Returns the section with the given section name.
     *
     * Returns the section structure of the given section in the given segment
     * in the mach executable it is linked into.
     *
     * ---
     * void main()
     * {
     *      import core.sys.darwin.mach.getsect;
     *      assert(getsectbyname("__TEXT", "__text"));
     * }
     * ---
     *
     * Params:
     *  segname = the name of the segment
     *  sectname = the name of the section
     *
     * Returns: a pointer to the section structure or `null` if it doesn't exist
     */
    const(Section)* getsectbyname(
        const scope char* segname,
        const scope char* sectname
    );

    /**
     * Returns the section data of section with the given section name.
     *
     * Returns the section data of the given section in the given segment in the
     * image pointed to by the given mach header.
     *
     * ---
     * void main()
     * {
     *      import core.sys.darwin.mach.getsect;
     *      import core.sys.darwin.crt_externs;
     *
     *      auto mph = _NSGetMachExecuteHeader();
     *      int size;
     *      assert(getsectiondata(mph, "__TEXT", "__text", &size));
     *      assert(size > 0);
     * }
     * ---
     *
     * Params:
     *  mhp = the mach header to get the section data from
     *  segname = the name of the segment
     *  sectname = the name of the section
     *  size = this will be set to the size of the section or 0 if the section
     *      doesn't exist
     *
     * Returns: a pointer to the section data or `null` if it doesn't exist
     */
    ubyte* getsectiondata(
        const scope MachHeader* mhp,
        const scope char* segname,
        const scope char* sectname,
        c_ulong* size
    );

    /**
     * Returns the segment with the given segment name.
     *
     * Returns the segment structure of the given segment in the mach executable
     * it is linked into.
     *
     * ---
     * void main()
     * {
     *      import core.sys.darwin.mach.getsect;
     *      assert(getsegbyname("__TEXT"));
     * }
     * ---
     *
     * Params:
     *  segname = the name of the segment
     *
     * Returns: a pointer to the section structure or `null` if it doesn't exist
     */
    const(SegmentCommand)* getsegbyname(
        const scope char* segname
    );

    /**
     * Returns the segment data of segment with the given segment name.
     *
     * Returns the segment data of the given segment in the image pointed to by
     * the given mach header.
     *
     * ---
     * void main()
     * {
     *      import core.sys.darwin.mach.getsect;
     *      import core.sys.darwin.crt_externs;
     *
     *      auto mph = _NSGetMachExecuteHeader();
     *      int size;
     *      assert(getsegmentdata(mph, "__TEXT", &size));
     *      assert(size > 0);
     * }
     * ---
     *
     * Params:
     *  mhp = the mach header to get the section data from
     *  segname = the name of the segment
     *  size = this will be set to the size of the section or 0 if the section
     *      doesn't exist
     *
     * Returns: a pointer to the section data or `null` if it doesn't exist
     */
    ubyte* getsegmentdata(
        const scope MachHeader* mhp,
        const scope char* segname,
        c_ulong* size
    );

    struct mach_header;
    struct mach_header_64;
    struct section;
    struct section_64;

    /**
     * Returns the section data of section with the given section name.
     *
     * Returns the section data of the given section in the given segment in the
     * image pointed to by the given mach header.
     *
     * ---
     * void main()
     * {
     *      import core.sys.darwin.mach.getsect;
     *      import core.sys.darwin.crt_externs;
     *
     *      auto mph = _NSGetMachExecuteHeader();
     *      int size;
     *      assert(getsectdatafromheader(mph, "__TEXT", "__text", &size));
     *      assert(size > 0);
     * }
     * ---
     *
     * Params:
     *  mhp = the mach header to get the section data from
     *  segname = the name of the segment
     *  sectname = the name of the section
     *  size = this will be set to the size of the section or 0 if the section
     *      doesn't exist
     *
     * Returns: a pointer to the section data or `null` if it doesn't exist
     */
    ubyte* getsectdatafromheader(
        const scope mach_header* mhp,
        const scope char* segname,
        const scope char* sectname,
        c_ulong* size
    );

    /// ditto
    ubyte* getsectdatafromheader_64(
        const scope mach_header_64* mhp,
        const scope char* segname,
        const scope char* sectname,
        c_ulong* size
    );


    /**
     * Returns the section with the given section name.
     *
     * Returns the section structure of the given section in the given segment
     * in image pointed to by the given mach header.
     *
     * ---
     * void main()
     * {
     *      import core.sys.darwin.mach.getsect;
     *      import core.sys.darwin.crt_externs;
     *
     *      auto mph = _NSGetMachExecuteHeader();
     *      assert(getsectbynamefromheader(mph, "__TEXT", "__text"));
     * }
     * ---
     *
     * Params:
     *  mhp = the mach header to get the section from
     *  segname = the name of the segment
     *  sectname = the name of the section
     *
     * Returns: a pointer to the section structure or `null` if it doesn't exist
     */
    const(section)* getsectbynamefromheader(
        const scope mach_header* mhp,
        const scope char* segname,
        const scope char* sectname
    );

    /// ditto
    const(section_64)* getsectbynamefromheader_64(
        const scope mach_header_64* mhp,
        const scope char* segname,
        const scope char* sectname
    );

    /**
     * Returns the section with the given section name.
     *
     * Returns the section structure of the given section in the given segment
     * in image pointed to by the given mach header.
     *
     * Params:
     *  mhp = the mach header to get the section from
     *  segname = the name of the segment
     *  section = the name of the section
     *  fSwap = ?
     *
     * Returns: a pointer to the section structure or `null` if it doesn't exist
     */
    const(section)* getsectbynamefromheaderwithswap(
        const scope mach_header* mhp,
        const scope char* segname,
        const scope char* section,
        int fSwap
    );

    /// ditto
    const(section)* getsectbynamefromheaderwithswap_64(
        const scope mach_header_64* mhp,
        const scope char* segname,
        const scope char* section,
        int fSwap
    );
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

public import core.sys.darwin.mach.loader;

import core.stdc.config : c_ulong;

char* getsectdata(
    const scope char* segname,
    const scope char* sectname,
    c_ulong *size
);

char* getsectdatafromFramework(
    const scope char* FrameworkName,
    const scope char* segname,
    const scope char* sectname,
    c_ulong* size
);

c_ulong get_end();
c_ulong get_etext();
c_ulong get_edata();

// Runtime interfaces for 64-bit Mach-O programs.
version (D_LP64)
{
    const(section_64)* getsectbyname(
        const scope char* segname,
        const scope char* sectname
    );

    ubyte* getsectiondata(
        const scope mach_header_64* mhp,
        const scope char* segname,
        const scope char* sectname,
        c_ulong* size
    );

    const(segment_command_64)* getsegbyname(
        const scope char* segname
    );

    ubyte* getsegmentdata(
        const scope mach_header_64* mhp,
        const scope char* segname,
        c_ulong* size
    );
}

// Runtime interfaces for 32-bit Mach-O programs.
else
{
    const(section)* getsectbyname(
        const scope char* segname,
        const scope char* sectname
    );

    ubyte* getsectiondata(
        const scope mach_header* mhp,
        const scope char* segname,
        const scope char* sectname,
        c_ulong* size
    );

    const(segment_command)* getsegbyname(
        const scope char* segname
    );

    ubyte* getsegmentdata(
        const scope mach_header* mhp,
        const scope char* segname,
        c_ulong* size
    );
}

// Interfaces for tools working with 32-bit Mach-O files.

ubyte* getsectdatafromheader(
    const scope mach_header* mhp,
    const scope char* segname,
    const scope char* sectname,
    c_ulong* size
);

const(section)* getsectbynamefromheader(
    const scope mach_header* mhp,
    const scope char* segname,
    const scope char* sectname
);

const(section)* getsectbynamefromheaderwithswap(
    const scope mach_header* mhp,
    const scope char* segname,
    const scope char* section,
    int fSwap
);

// Interfaces for tools working with 64-bit Mach-O files.

ubyte* getsectdatafromheader_64(
    const scope mach_header_64* mhp,
    const scope char* segname,
    const scope char* sectname,
    c_ulong* size
);

const(section_64)* getsectbynamefromheader_64(
    const scope mach_header_64* mhp,
    const scope char* segname,
    const scope char* sectname
);

const(section)* getsectbynamefromheaderwithswap_64(
    const scope mach_header_64* mhp,
    const scope char* segname,
    const scope char* section,
    int fSwap
);
