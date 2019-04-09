/**
 * D header file for $(LINK2 https://opensource.apple.com/source/Libc/Libc-1244.30.3/include/crt_externs.h.auto.html, libc/crt_externs.h).
 *
 * Copyright: Copyright (c) 2018 D Language Foundation
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Jacob Carlborg
 * Source:    $(DRUNTIMESRC core/sys/darwin/_crt_externs.d)
 */
module core.sys.darwin.crt_externs;

version (CoreDoc)
{
    /**
     * In reality this will be $(REF mach_header, core, sys, darwin, mach, loader)
     * on 32-bit platforms and $(REF mach_header_64, core, sys, darwin, mach, loader)
     * 64-bit platforms.
     */
    struct MachHeader;

    /**
     * Returns the program arguments.
     *
     * These are the same arguments passed to the C main function:
     *
     * ___
     * extern (C) void main (char** argv, int argc, char** envp) {}
     * ___
     *
     * Same as the above `argv`.
     *
     * Return: the program arguments as a pointer to an array of null terminated C
     *  strings
     */
    char*** _NSGetArgv();

    /**
     * Returns the number of program arguments.
     *
     * These are the same arguments passed to the C main function:
     *
     * ___
     * extern (C) void main (char** argv, int argc, char** envp) {}
     * ___
     *
     * Same as the above `argc`.
     *
     * Return: a pointer to the number of program arguments
     */
    int* _NSGetArgc();

    /**
     * Returns the program environment variables.
     *
     * These are the same arguments passed as an array to the C main function:
     *
     * ___
     * extern (C) void main (char** argv, int argc, char** envp) {}
     * ___
     *
     * Same as the above `envp`.
     *
     * Return: the program environment variables as a pointer to an array of null
     *  terminated C strings
     */
    char*** _NSGetEnviron();

    /**
     * Returns the full path to the current executable as a pointer to a null
     * terminated C string.
     */
    char** _NSGetProgname();

    /// Returns the Mach-O header of the current executable.
    MachHeader* _NSGetMachExecuteHeader();
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
extern(C):
nothrow:
@nogc:

import core.sys.darwin.mach.loader : mach_header, mach_header_64;

char*** _NSGetArgv();
int* _NSGetArgc();
char*** _NSGetEnviron();
char** _NSGetProgname();

version (D_LP64)
    mach_header_64* _NSGetMachExecuteHeader();
else
    mach_header* _NSGetMachExecuteHeader();
