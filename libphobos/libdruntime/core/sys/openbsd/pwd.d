/**
  * D header file for OpenBSD pwd.h.
  *
  * Copyright: Copyright © 2022, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Brian Callahan
  */
module core.sys.openbsd.pwd;

version (OpenBSD):
extern (C):
nothrow:
@nogc:

public import core.sys.posix.pwd;
import core.sys.posix.sys.types : uid_t;

passwd* getpwnam_shadow(scope const char*);
passwd* getpwuid_shadow(uid_t);
