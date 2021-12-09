// Written in the D programming language.

/**
 * Information about the target operating system, environment, and CPU.
 *
 *  Copyright: Copyright The D Language Foundation 2000 - 2011
 *  License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *  Authors:   $(HTTP digitalmars.com, Walter Bright) and
               $(HTTP jmdavisprog.com, Jonathan M Davis)
 *  Source:    $(PHOBOSSRC std/system.d)
 */
module std.system;

immutable
{
    /++
        Operating system.

        Note:
            This is for cases where you need a value representing the OS at
            runtime. If you're doing something which should compile differently
            on different OSes, then please use `version (Windows)`,
            `version (linux)`, etc.

        See_Also:
            $(DDSUBLINK spec/version,PredefinedVersions, Predefined Versions)
      +/
    enum OS
    {
        win32 = 1, /// Microsoft 32 bit Windows systems
        win64,     /// Microsoft 64 bit Windows systems
        linux,     /// All Linux Systems, except for Android
        osx,       /// Mac OS X
        iOS,       /// iOS
        tvOS,      /// tvOS
        watchOS,   /// watchOS
        freeBSD,   /// FreeBSD
        netBSD,    /// NetBSD
        openBSD,   /// OpenBSD
        dragonFlyBSD, /// DragonFlyBSD
        solaris,   /// Solaris
        android,   /// Android
        otherPosix, /// Other Posix Systems
        unknown,   /// Unknown
    }

    /// The OS that the program was compiled for.
    version (Win32)        OS os = OS.win32;
    else version (Win64)   OS os = OS.win64;
    else version (Android) OS os = OS.android;
    else version (linux)   OS os = OS.linux;
    else version (OSX)     OS os = OS.osx;
    else version (iOS)     OS os = OS.iOS;
    else version (tvOS)    OS os = OS.tvOS;
    else version (watchOS) OS os = OS.watchOS;
    else version (FreeBSD) OS os = OS.freeBSD;
    else version (NetBSD)  OS os = OS.netBSD;
    else version (OpenBSD) OS os = OS.openBSD;
    else version (DragonFlyBSD) OS os = OS.dragonFlyBSD;
    else version (Posix)   OS os = OS.otherPosix;
    else OS os = OS.unknown;

    /++
        Byte order endianness.

        Note:
            This is intended for cases where you need to deal with endianness at
            runtime. If you're doing something which should compile differently
            depending on whether you're compiling on a big endian or little
            endian machine, then please use `version (BigEndian)` and
            `version (LittleEndian)`.

        See_Also:
            $(DDSUBLINK spec/version,PredefinedVersions, Predefined Versions)
      +/
    enum Endian
    {
        bigEndian,   /// Big endian byte order
        littleEndian /// Little endian byte order
    }

    /// The endianness that the program was compiled for.
    version (LittleEndian) Endian endian = Endian.littleEndian;
    else                  Endian endian = Endian.bigEndian;
}

