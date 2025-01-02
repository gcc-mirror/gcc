/* DWARF2 EH unwinding support for FreeBSD/ARM64 (aarch64).
   Copyright (C) 2017-2025 Free Software Foundation, Inc.
   Contributed by John Marino <gnugcc@marino.st>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Identify a signal frame, and set the frame state data appropriately.
   See unwind-dw2.c for the structs. */

/* Always include AArch64 unwinder header file.  */
#include "config/aarch64/aarch64-unwind.h"

#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include <sys/ucontext.h>
#include <machine/frame.h>
#include <sys/user.h>
#include <sys/sysctl.h>

#define REG_NAME(reg)   mc_gpregs.gp_## reg
#define XREG(num)       mc_gpregs.gp_x[num]
#define DARC            __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__

#define MD_FALLBACK_FRAME_STATE_FOR aarch64_freebsd_fallback_frame_state

static int
aarch64_outside_sigtramp_range (unsigned char *pc)
{
  static int sigtramp_range_determined = 0;
  static unsigned char *sigtramp_start, *sigtramp_end;

  if (sigtramp_range_determined == 0)
    {
      struct kinfo_sigtramp kst = {0};
      size_t len = sizeof (kst);
      int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_SIGTRAMP, getpid() };

      sigtramp_range_determined = 1;
      if (sysctl (mib, 4, &kst, &len, NULL, 0) == 0)
      {
        sigtramp_range_determined = 2;
        sigtramp_start = kst.ksigtramp_start;
        sigtramp_end   = kst.ksigtramp_end;
      }
    }
  if (sigtramp_range_determined < 2)  /* sysctl failed if < 2 */
    return 1;

  return (pc < sigtramp_start || pc >= sigtramp_end);
}

static _Unwind_Reason_Code
aarch64_freebsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  int n;
  struct sigframe *sf;
  mcontext_t *sc;
  _Unwind_Ptr new_cfa;

  if (aarch64_outside_sigtramp_range(context->ra))
    return _URC_END_OF_STACK;

  sf = (struct sigframe *) context->cfa;
  sc = &sf->sf_uc.uc_mcontext;

  new_cfa = (_Unwind_Ptr) sc;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  for (n = 0; n < 32; n++)
    fs->regs.how[n] = REG_SAVED_OFFSET;

  for (n = 0; n < 30; n++)
    fs->regs.reg[n].loc.offset = (_Unwind_Ptr) &(sc->XREG(n)) - new_cfa;

  fs->regs.reg[30].loc.offset = (_Unwind_Ptr) &(sc->REG_NAME(lr)) - new_cfa;
  fs->regs.reg[31].loc.offset = (_Unwind_Ptr) &(sc->REG_NAME(sp)) - new_cfa;

  fs->regs.how[DARC] = REG_SAVED_OFFSET;
  fs->regs.reg[DARC].loc.offset = (_Unwind_Ptr) &(sc->REG_NAME(elr)) - new_cfa;

  fs->retaddr_column = DARC;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}
