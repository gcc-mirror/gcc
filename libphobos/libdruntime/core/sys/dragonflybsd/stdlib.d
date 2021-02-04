/**
  * D header file for DragonFlyBSD stdlib.h.
  *
  * Copyright: Copyright © 2021, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Iain Buclaw
  */
module core.sys.dragonflybsd.stdlib;
public import core.sys.posix.stdlib;

version (DragonFlyBSD):
extern (C):
nothrow:
@nogc:

const(char)* getprogname();
void setprogname(scope const char* name);
