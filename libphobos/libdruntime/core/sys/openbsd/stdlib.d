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

void freezero(void*, size_t);
void* calloc_conceal(size_t, size_t);
void* malloc_conceal(size_t);
void* reallocarray(void*, size_t, size_t);
void* recallocarray(void*, size_t, size_t, size_t);

const(char)* getprogname();
void setprogname(scope const char*);
