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
    void explicit_bzero(void*, size_t);
    pure void* memmem(return scope const void* haystack, size_t haystacklen, scope const void* needle, size_t needlelen);
    void* memrchr(scope const void*, int, size_t);
    size_t strlcat(char*, scope const char*, size_t);
    size_t strlcpy(char*, scope const char*, size_t);
    void strmode(int, char*);
    char* strsep(char**, scope const char*);
    pure int timingsafe_bcmp(scope const void*, scope const void*, size_t);
    pure int timingsafe_memcmp(scope const void*, scope const void*, size_t);
}
