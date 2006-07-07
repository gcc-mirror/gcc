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
  register _Jv_uintptr_t *_ebp __asm__ ("ebp");
  register _Jv_uintptr_t _esp __asm__ ("esp");
  _Jv_uintptr_t rfp;

  int i = state->pos;
  for (rfp = *_ebp;
       rfp && i < state->length;
       rfp = *(_Jv_uintptr_t *)rfp)
    {
      /* Sanity checks to eliminate dubious-looking frame pointer chains.
         The frame pointer should be a 32-bit word-aligned stack address.
         Since the stack grows downwards on x86, the frame pointer must have
         a value greater than the current value of the stack pointer, it
         should not be below the supposed next frame pointer and it should
         not be too far off from the supposed next frame pointer.  */
      int diff = *(_Jv_uintptr_t *)rfp - rfp;
      if ((rfp & 0x00000003) != 0 || rfp < _esp
          || diff > 4 * 1024 || diff < 0)
        break;

      /* Get the return address in the calling function.  This is stored on
         the stack just before the value of the old frame pointer.  */
      _Jv_uintptr_t ret_addr
        = *(_Jv_uintptr_t *)(rfp + sizeof (_Jv_uintptr_t));

      state->frames[i].type = frame_native;
      state->frames[i].ip = (void *)(ret_addr - 1);
      state->frames[i].start_ip = NULL;

      /* Try to locate a "pushl %ebp; movl %esp, %ebp" function prologue
         by scanning backwards at even addresses below the return address.
         This instruction sequence is encoded as 0x55 0x89 0xE5.  We give up
         if we do not find this sequence even after scanning 1024K of memory.
         FIXME: This is not robust and will probably give us false positives,
         but this is about the best we can do if we do not have DWARF-2 unwind
         information based exception handling.  */
      _Jv_uintptr_t scan_addr = (ret_addr & 0xFFFFFFFE) - 2;
      _Jv_uintptr_t limit_addr
        = (scan_addr > 1024 * 1024) ? (scan_addr - 1024 * 1024) : 2;
      for ( ; scan_addr >= limit_addr; scan_addr -= 2)
        {
          unsigned char *scan_bytes = (unsigned char *)scan_addr;
          if (scan_bytes[0] == 0x55 && scan_bytes[1] == 0x89
              && scan_bytes[2] == 0xE5)
            {
              state->frames[i].start_ip = (void *)scan_addr;
              break;
            }
        }

      /* No need to unwind beyond JvRunMain().  */
      if (state->frames[i].start_ip == (void *)JvRunMain)
        break;

      i++;
    }
  state->pos = i;
}
#endif
