/**
  * D header file for FreeBSD string.
  *
  * Copyright: Copyright © 2019, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Ernesto Castellotti
  */
module core.sys.freebsd.string;

public import core.stdc.string;
import core.sys.freebsd.sys.cdefs;

version (FreeBSD):
extern (C):
nothrow:
@nogc:

static if (__BSD_VISIBLE)
{
    pure void* memmem(return scope const void* haystack, size_t haystacklen, scope const void* needle, size_t needlelen);
}
