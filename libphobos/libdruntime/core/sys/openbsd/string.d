/**
  * D header file for OpenBSD string.
  *
  * Copyright: Copyright © 2019, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Ernesto Castellotti
  */
module core.sys.openbsd.string;

public import core.stdc.string;
import core.sys.openbsd.sys.cdefs;

version (OpenBSD):
extern (C):
nothrow:
@nogc:

static if (__BSD_VISIBLE)
{
    pure void* memmem(return const void* haystack, size_t haystacklen, scope const void* needle, size_t needlelen);
    pure int timingsafe_bcmp(scope const void*, scope const void*, size_t);
    pure int timingsafe_memcmp(scope const void*, scope const void*, size_t);
}
