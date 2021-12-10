/**
  * D header file for Bionic string.
  *
  * Copyright: Copyright © 2019, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Ernesto Castellotti
  */
module core.sys.bionic.string;

public import core.stdc.string;

version (CRuntime_Bionic):
extern (C):
nothrow:
@nogc:

pure void* memmem(return scope const void* haystack, size_t haystacklen, scope const void* needle, size_t needlelen);
