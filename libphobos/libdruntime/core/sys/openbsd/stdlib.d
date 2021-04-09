/**
  * D header file for OpenBSD stdlib.h.
  *
  * Copyright: Copyright © 2021, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Iain Buclaw
  */
module core.sys.openbsd.stdlib;
public import core.sys.posix.stdlib;

version (OpenBSD):
extern (C):
nothrow:
@nogc:

const(char)* getprogname();
void setprogname(scope const char* name);
