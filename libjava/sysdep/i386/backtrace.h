// backtrace.h - Fallback backtrace implementation. i386 implementation.

/* Copyright (C) 2005, 2006  Free Software Foundation

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
  _Jv_uintptr_t *rfp;

  int i = state->pos;
  for (rfp = *(_Jv_uintptr_t **)_ebp;
       rfp && i < state->length;
       rfp = *(_Jv_uintptr_t **)rfp)
    {
      /* Sanity checks to eliminate dubious-looking frame pointer chains.
         The frame pointer should be a 32-bit word-aligned stack address.
         Since the stack grows downwards on x86, the frame pointer must have
         a value greater than the current value of the stack pointer, it
         should not be below the supposed next frame pointer and it should
         not be too far off from the supposed next frame pointer.  */
      int diff = *rfp - (_Jv_uintptr_t)rfp;
      if (((_Jv_uintptr_t)rfp & 0x00000003) != 0 || (void*)rfp < _esp
          || diff > 4 * 1024 || diff < 0)
        break;

      /* Use the return address in the calling function stored just before
         the current frame pointer to locate the address operand part of the
         "CALL <XYZ>" instruction in the calling function that called this
         function.  */
      void *ip = (void*)(rfp[1] - 4);

      /* Verify that the instruction at this position is a "CALL <XYZ>" and
         use its operand to determine the starting address of the function
         that this function had called.  0xE8 is the opcode for this CALL
         instruction variant.  */
      if (*(unsigned char *)((_Jv_uintptr_t)ip - 1) == 0xE8 && i > state->pos
          && state->frames[i-1].type == frame_native)
        {
          state->frames[i-1].start_ip
            = (void *)((_Jv_uintptr_t)ip + 4 + *(_Jv_uintptr_t *)ip);
        }

      state->frames[i].type = frame_native;
      state->frames[i].ip = ip;

      i++;
    }
  state->pos = i;
}
#endif
