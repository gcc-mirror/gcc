/**
 * D header file for Darwin.
 *
 * $(LINK2 https://opensource.apple.com/source/dyld/dyld-360.22/include/dlfcn.h, Apple dyld/dlfcn.h)
 *
 * Copyright: Copyright David Nadlinger 2016.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   David Nadlinger
 */
module core.sys.darwin.dlfcn;

version (OSX)
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

public import core.sys.posix.dlfcn;

enum RTLD_NEXT = cast(void*) -1;
enum RTLD_DEFAULT = cast(void*) -2;
enum RTLD_SELF = cast(void*) -3;
enum RTLD_MAIN_ONLY = cast(void*) -5;
