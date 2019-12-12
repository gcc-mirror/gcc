/**
  * D header file for Darwin string.
  *
  * Copyright: Copyright © 2019, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Ernesto Castellotti
  */
module core.sys.darwin.string;

public import core.stdc.string;
import core.sys.darwin.sys.cdefs;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
extern (C):
nothrow:
@nogc:

static if (__DARWIN_C_LEVEL >= __DARWIN_C_FULL)
{
    // ^ __OSX_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_4_3);
    pure void* memmem(return const void* haystack, size_t haystacklen, scope const void* needle, size_t needlelen);
}
