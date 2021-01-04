/* DWARF2 EH unwinding support for DragonFly BSD: AMD x86-64 and x86.
   Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs. */

#include <sys/types.h>
#include <sys/sysctl.h>
#include <sys/param.h>
#include <signal.h>
#include <sys/ucontext.h>
#include <machine/sigframe.h>
#if __DragonFly_version > 400800
#include <sys/kinfo.h>
#endif


#define REG_NAME(reg)	sf_uc.uc_mcontext.mc_## reg

#ifdef __x86_64__
#define MD_FALLBACK_FRAME_STATE_FOR x86_64_dragonfly_fallback_frame_state


static int
x86_64_outside_sigtramp_range (unsigned char *pc)
{
  static int sigtramp_range_determined = 0;
  static unsigned char *sigtramp_start, *sigtramp_end;

  if (sigtramp_range_determined == 0)
    {
#if __DragonFly_version > 400800
      struct kinfo_sigtramp kst = {0};
      size_t len = sizeof (kst);
      int mib[3] = { CTL_KERN, KERN_PROC, KERN_PROC_SIGTRAMP };

      sigtramp_range_determined = 1;
      if (sysctl (mib, 3, &kst, &len, NULL, 0) == 0)
      {
        sigtramp_range_determined = 2;
        sigtramp_start = kst.ksigtramp_start;
        sigtramp_end   = kst.ksigtramp_end;
      }
#else
      unsigned long ps_strings;
      size_t len = sizeof (ps_strings);
      int mib[2] = { CTL_KERN, KERN_PS_STRINGS };

      sigtramp_range_determined = 1;
      if (sysctl (mib, 2, &ps_strings, &len, NULL, 0) == 0)
      {
        sigtramp_range_determined = 2;
        sigtramp_start = (unsigned char *)ps_strings - 32;
        sigtramp_end   = (unsigned char *)ps_strings;
      }
#endif
    }
  if (sigtramp_range_determined < 2)  /* sysctl failed if < 2 */
    return 1;

  return (pc < sigtramp_start || pc >= sigtramp_end );
}


static _Unwind_Reason_Code
x86_64_dragonfly_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct sigframe *sf;
  long new_cfa;

  if (x86_64_outside_sigtramp_range(context->ra))
    return _URC_END_OF_STACK;

  sf = (struct sigframe *) context->cfa;
  new_cfa = sf->REG_NAME(rsp);
  fs->regs.cfa_how = CFA_REG_OFFSET;
  /* Register 7 is rsp  */
  fs->regs.cfa_reg = 7;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sf->REG_NAME(rax) - new_cfa;
  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sf->REG_NAME(rdx) - new_cfa;
  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sf->REG_NAME(rcx) - new_cfa;
  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sf->REG_NAME(rbx) - new_cfa;
  fs->regs.reg[4].how = REG_SAVED_OFFSET;
  fs->regs.reg[4].loc.offset = (long)&sf->REG_NAME(rsi) - new_cfa;
  fs->regs.reg[5].how = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sf->REG_NAME(rdi) - new_cfa;
  fs->regs.reg[6].how = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sf->REG_NAME(rbp) - new_cfa;
  fs->regs.reg[8].how = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sf->REG_NAME(r8) - new_cfa;
  fs->regs.reg[9].how = REG_SAVED_OFFSET;
  fs->regs.reg[9].loc.offset = (long)&sf->REG_NAME(r9) - new_cfa;
  fs->regs.reg[10].how = REG_SAVED_OFFSET;
  fs->regs.reg[10].loc.offset = (long)&sf->REG_NAME(r10) - new_cfa;
  fs->regs.reg[11].how = REG_SAVED_OFFSET;
  fs->regs.reg[11].loc.offset = (long)&sf->REG_NAME(r11) - new_cfa;
  fs->regs.reg[12].how = REG_SAVED_OFFSET;
  fs->regs.reg[12].loc.offset = (long)&sf->REG_NAME(r12) - new_cfa;
  fs->regs.reg[13].how = REG_SAVED_OFFSET;
  fs->regs.reg[13].loc.offset = (long)&sf->REG_NAME(r13) - new_cfa;
  fs->regs.reg[14].how = REG_SAVED_OFFSET;
  fs->regs.reg[14].loc.offset = (long)&sf->REG_NAME(r14) - new_cfa;
  fs->regs.reg[15].how = REG_SAVED_OFFSET;
  fs->regs.reg[15].loc.offset = (long)&sf->REG_NAME(r15) - new_cfa;
  fs->regs.reg[16].how = REG_SAVED_OFFSET;
  fs->regs.reg[16].loc.offset = (long)&sf->REG_NAME(rip) - new_cfa;
  fs->retaddr_column = 16;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}

#else /* Next section is for i386  */

#define MD_FALLBACK_FRAME_STATE_FOR x86_dragonfly_fallback_frame_state


static void
x86_sigtramp_range (unsigned char **start, unsigned char **end)
{
  unsigned long ps_strings;
  int mib[2];
  size_t len;

  mib[0] = CTL_KERN;
  mib[1] = KERN_PS_STRINGS;
  len = sizeof (ps_strings);
  sysctl (mib, 2, &ps_strings, &len, NULL, 0);

  *start = (unsigned char *)ps_strings - 128;
  *end   = (unsigned char *)ps_strings;
}


static _Unwind_Reason_Code
x86_dragonfly_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  unsigned char *sigtramp_start, *sigtramp_end;
  struct sigframe *sf;
  long new_cfa;

  x86_sigtramp_range(&sigtramp_start, &sigtramp_end);

  if (pc >= sigtramp_end || pc < sigtramp_start)
    return _URC_END_OF_STACK;

  sf = (struct sigframe *) context->cfa;
  new_cfa = sf->REG_NAME(esp);
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 4;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sf->REG_NAME(eax) - new_cfa;
  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sf->REG_NAME(ebx) - new_cfa;
  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sf->REG_NAME(ecx) - new_cfa;
  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sf->REG_NAME(edx) - new_cfa;
  fs->regs.reg[6].how = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sf->REG_NAME(esi) - new_cfa;
  fs->regs.reg[7].how = REG_SAVED_OFFSET;
  fs->regs.reg[7].loc.offset = (long)&sf->REG_NAME(edi) - new_cfa;
  fs->regs.reg[5].how = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sf->REG_NAME(ebp) - new_cfa;
  fs->regs.reg[8].how = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sf->REG_NAME(eip) - new_cfa;
  fs->retaddr_column = 8;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}
#endif /* ifdef __x86_64__  */
