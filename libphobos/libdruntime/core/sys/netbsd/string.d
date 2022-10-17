/**
  * D header file for NetBSD string.
  *
  * Copyright: Copyright © 2019, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Ernesto Castellotti
  */
module core.sys.netbsd.string;

public import core.stdc.string;
import core.sys.netbsd.sys.featuretest;

version (NetBSD):
extern (C):
nothrow:
@nogc:

static if (_NETBSD_SOURCE)
{
    pure void* memmem(return scope const void* haystack, size_t haystacklen, scope const void* needle, size_t needlelen);
}
