/* errno.c -- functions for getting and setting errno

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <errno.h>
#include <stdint.h>

/* errno is typically a macro. These functions set 
   and get errno specific to the libc being used.  */

uintptr_t GetErrno() asm ("libgo_syscall.syscall.GetErrno");
void SetErrno(uintptr_t) asm ("libgo_syscall.syscall.SetErrno");

uintptr_t
GetErrno()
{
  return (uintptr_t) errno;
}

void
SetErrno(uintptr_t value)
{
  errno = (int) value;
}
