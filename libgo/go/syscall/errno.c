/* errno.c -- functions for getting and setting errno

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <errno.h>

/* errno is typically a macro. These functions set 
   and get errno specific to the libc being used.  */

int GetErrno() asm ("libgo_syscall.syscall.GetErrno");
void SetErrno(int) asm ("libgo_syscall.syscall.SetErrno");

int 
GetErrno()
{
  return errno;
}

void
SetErrno(int value)
{
  errno = value;
}
