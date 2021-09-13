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

struct Dl_info
{
    const(char)* dli_fname;
    void*        dli_fbase;
    const(char)* dli_sname;
    void*        dli_saddr;
}

int dladdr(const scope void* addr, Dl_info* info);

enum RTLD_NOLOAD = 0x10;
enum RTLD_NODELETE = 0x80;
enum RTLD_FIRST = 0x100;
