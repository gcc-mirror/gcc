/* errno.c -- functions for getting and setting errno

   Copyright 2022 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */
#include <errno.h>
#include <stdint.h>

#include "runtime.h"

/* errno is typically a macro. These functions set and get errno
   specific to the libc being used.  */

uintptr_t getErrno(void) __asm__ (GOSYM_PREFIX "runtime_1internal_1syscall.getErrno");
void setErrno(uintptr_t) __asm__ (GOSYM_PREFIX "runtime_1internal_1syscall.setErrno");

uintptr_t
getErrno(void)
{
  return (uintptr_t) errno;
}

void
setErrno(uintptr_t value)
{
  errno = (int) value;
}
