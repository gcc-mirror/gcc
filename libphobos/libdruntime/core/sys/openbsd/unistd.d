/**
  * D header file for OpenBSD unistd.h.
  *
  * Copyright: Copyright © 2021, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Brian Callahan
  */
module core.sys.openbsd.unistd;
public import core.sys.posix.unistd;

version (OpenBSD):
extern (C):
nothrow:
@nogc:

int pledge(const scope char*, const scope char*);
int unveil(const scope char*, const scope char*);
