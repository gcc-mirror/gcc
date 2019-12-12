/* wait.c -- functions for getting wait status values.

   Copyright 2011 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.

   We use C code to extract the wait status so that we can easily be
   OS-independent.  */

#include <stdint.h>
#include <sys/wait.h>

#include "runtime.h"

#ifndef WCOREDUMP
#define WCOREDUMP(status) (((status) & 0200) != 0)
#endif

#ifndef WIFCONTINUED
#define WIFCONTINUED(x) 0
#endif

extern _Bool Exited (uint32_t *w)
  __asm__ (GOSYM_PREFIX "syscall.WaitStatus.Exited");

_Bool
Exited (uint32_t *w)
{
  return WIFEXITED (*w) != 0;
}

extern _Bool Signaled (uint32_t *w)
  __asm__ (GOSYM_PREFIX "syscall.WaitStatus.Signaled");

_Bool
Signaled (uint32_t *w)
{
  return WIFSIGNALED (*w) != 0;
}

extern _Bool Stopped (uint32_t *w)
  __asm__ (GOSYM_PREFIX "syscall.WaitStatus.Stopped");

_Bool
Stopped (uint32_t *w)
{
  return WIFSTOPPED (*w) != 0;
}

extern _Bool Continued (uint32_t *w)
  __asm__ (GOSYM_PREFIX "syscall.WaitStatus.Continued");

_Bool
Continued (uint32_t *w __attribute__ ((unused)))
{
  return WIFCONTINUED (*w) != 0;
}

extern _Bool CoreDump (uint32_t *w)
  __asm__ (GOSYM_PREFIX "syscall.WaitStatus.CoreDump");

_Bool
CoreDump (uint32_t *w)
{
  return WCOREDUMP (*w) != 0;
}

extern intgo ExitStatus (uint32_t *w)
  __asm__ (GOSYM_PREFIX "syscall.WaitStatus.ExitStatus");

intgo
ExitStatus (uint32_t *w)
{
  if (!WIFEXITED (*w))
    return -1;
  return WEXITSTATUS (*w);
}

extern intgo Signal (uint32_t *w)
  __asm__ (GOSYM_PREFIX "syscall.WaitStatus.Signal");

intgo
Signal (uint32_t *w)
{
  if (!WIFSIGNALED (*w))
    return -1;
  return WTERMSIG (*w);
}

extern intgo StopSignal (uint32_t *w)
  __asm__ (GOSYM_PREFIX "syscall.WaitStatus.StopSignal");

intgo
StopSignal (uint32_t *w)
{
  if (!WIFSTOPPED (*w))
    return -1;
  return WSTOPSIG (*w);
}

extern intgo TrapCause (uint32_t *w)
  __asm__ (GOSYM_PREFIX "syscall.WaitStatus.TrapCause");

intgo
TrapCause (uint32_t *w __attribute__ ((unused)))
{
#ifndef __linux__
  return -1;
#else
  if (!WIFSTOPPED (*w) || WSTOPSIG (*w) != SIGTRAP)
    return -1;
  return *w >> 16;
#endif
}
