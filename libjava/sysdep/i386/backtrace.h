// backtrace.h - Fallback backtrace implementation. i386 implementation.

/* Copyright (C) 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_BACKTRACE_H__
#define __SYSDEP_BACKTRACE_H__

#include <java-stack.h>

#define HAVE_FALLBACK_BACKTRACE

/* Store return addresses of the current program stack in
   STATE and return the exact number of values stored.  */
void
fallback_backtrace (_Jv_UnwindState *state)
{
  register void *_ebp __asm__ ("ebp");
  register void *_esp __asm__ ("esp");
  unsigned int *rfp;

  int i = state->pos;
  for (rfp = *(unsigned int**)_ebp;
       rfp && i < state->length;
       rfp = *(unsigned int **)rfp)
    {
      int diff = *rfp - (unsigned int)rfp;
      if ((void*)rfp < _esp || diff > 4 * 1024 || diff < 0)
        break;

      state->frames[i].type = frame_native;
      state->frames[i].ip = (void*)(rfp[1]-4);
      i++;
    }
  state->pos = i;
}
#endif
