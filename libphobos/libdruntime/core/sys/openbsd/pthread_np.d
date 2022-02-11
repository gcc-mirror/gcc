/**
  * D header file for OpenBSD pthread_np.h.
  *
  * Copyright: Copyright © 2021, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Brian Callahan
  */
module core.sys.openbsd.pthread_np;

version (OpenBSD):
extern (C):
nothrow:
@nogc:

public import core.sys.posix.sys.types;
import core.sys.posix.signal : stack_t;

int pthread_mutexattr_getkind_np(pthread_mutexattr_t);
int pthread_mutexattr_setkind_np(pthread_mutexattr_t*, int);
void pthread_get_name_np(pthread_t, char*, size_t);
void pthread_set_name_np(pthread_t, const(char)*);
int pthread_stackseg_np(pthread_t, stack_t*);
int pthread_main_np();
