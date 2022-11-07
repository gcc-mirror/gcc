/* DWARF2 EH unwinding support for FreeBSD: AMD x86-64 and x86.
   Copyright (C) 2015-2022 Free Software Foundation, Inc.
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
#include <signal.h>
#include <unistd.h>
#include <sys/sysctl.h>
#include <sys/ucontext.h>
#include <sys/user.h>
#include <machine/sigframe.h>

#define REG_NAME(reg)	sf_uc.uc_mcontext.mc_## reg

#ifdef __x86_64__
#define MD_FALLBACK_FRAME_STATE_FOR x86_64_freebsd_fallback_frame_state

#ifdef KERN_PROC_SIGTRAMP
/* FreeBSD past 9.3 provides a kern.proc.sigtramp.<pid> sysctl that
   returns the location of the signal trampoline. Use this to find
   out whether we're in a trampoline.
*/
static int
x86_64_outside_sigtramp_range (unsigned char *pc)
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
#endif

static _Unwind_Reason_Code
x86_64_freebsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct sigframe *sf;
  long new_cfa;

#ifndef KERN_PROC_SIGTRAMP
  /* Prior to FreeBSD 9, the signal trampoline was located immediately
     before the ps_strings.  To support non-executable stacks on AMD64,
     the sigtramp was moved to a shared page for FreeBSD 9.  Unfortunately
     this means looking frame patterns again (sys/amd64/amd64/sigtramp.S)
     rather than using the robust and convenient KERN_PS_STRINGS trick.

     <pc + 00>:  lea     0x10(%rsp),%rdi
     <pc + 05>:  pushq   $0x0
     <pc + 17>:  mov     $0x1a1,%rax
     <pc + 14>:  syscall

     If we can't find this pattern, we're at the end of the stack.
  */

  if (!(   *(unsigned int *)(context->ra)      == 0x247c8d48
        && *(unsigned int *)(context->ra +  4) == 0x48006a10
        && *(unsigned int *)(context->ra +  8) == 0x01a1c0c7
        && *(unsigned int *)(context->ra + 12) == 0x050f0000 ))
    return _URC_END_OF_STACK;
#else
  if (x86_64_outside_sigtramp_range(context->ra))
    return _URC_END_OF_STACK;
#endif

  sf = (struct sigframe *) context->cfa;
  new_cfa = sf->REG_NAME(rsp);
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg =  __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.how[0] = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sf->REG_NAME(rax) - new_cfa;
  fs->regs.how[1] = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sf->REG_NAME(rdx) - new_cfa;
  fs->regs.how[2] = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sf->REG_NAME(rcx) - new_cfa;
  fs->regs.how[3] = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sf->REG_NAME(rbx) - new_cfa;
  fs->regs.how[4] = REG_SAVED_OFFSET;
  fs->regs.reg[4].loc.offset = (long)&sf->REG_NAME(rsi) - new_cfa;
  fs->regs.how[5] = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sf->REG_NAME(rdi) - new_cfa;
  fs->regs.how[6] = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sf->REG_NAME(rbp) - new_cfa;
  fs->regs.how[8] = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sf->REG_NAME(r8) - new_cfa;
  fs->regs.how[9] = REG_SAVED_OFFSET;
  fs->regs.reg[9].loc.offset = (long)&sf->REG_NAME(r9) - new_cfa;
  fs->regs.how[10] = REG_SAVED_OFFSET;
  fs->regs.reg[10].loc.offset = (long)&sf->REG_NAME(r10) - new_cfa;
  fs->regs.how[11] = REG_SAVED_OFFSET;
  fs->regs.reg[11].loc.offset = (long)&sf->REG_NAME(r11) - new_cfa;
  fs->regs.how[12] = REG_SAVED_OFFSET;
  fs->regs.reg[12].loc.offset = (long)&sf->REG_NAME(r12) - new_cfa;
  fs->regs.how[13] = REG_SAVED_OFFSET;
  fs->regs.reg[13].loc.offset = (long)&sf->REG_NAME(r13) - new_cfa;
  fs->regs.how[14] = REG_SAVED_OFFSET;
  fs->regs.reg[14].loc.offset = (long)&sf->REG_NAME(r14) - new_cfa;
  fs->regs.how[15] = REG_SAVED_OFFSET;
  fs->regs.reg[15].loc.offset = (long)&sf->REG_NAME(r15) - new_cfa;
  fs->regs.how[16] = REG_SAVED_OFFSET;
  fs->regs.reg[16].loc.offset = (long)&sf->REG_NAME(rip) - new_cfa;
  fs->retaddr_column = 16;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}

#else /* Next section is for i386  */

#define MD_FALLBACK_FRAME_STATE_FOR x86_freebsd_fallback_frame_state

/*
 * We can't use KERN_PS_STRINGS anymore if we want to support FreeBSD32
 * compat on AMD64.  The sigtramp is in a shared page in that case so the
 * x86_sigtramp_range only works on a true i386 system.  We have to
 * search for the sigtramp frame if we want it working everywhere.
 */

static _Unwind_Reason_Code
x86_freebsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct sigframe *sf;
  long new_cfa;

/*
 * i386 sigtramp frame we are looking for follows.
 * Apparently PSL_VM is variable, so we can't look past context->ra + 4
 * <sigcode>:
 *   0:	ff 54 24 10          	call   *0x10(%esp)          *SIGF_HANDLER
 *   4:	8d 44 24 20          	lea    0x20(%esp),%eax       SIGF_UC
 *   8:	50                   	push   %eax
 *   9:	f7 40 54 00 00 02 00 	testl  $0x20000,0x54(%eax)  $PSL_VM
 *  10:	75 03                	jne    15 <sigcode+0x15>
 *  12:	8e 68 14             	mov    0x14(%eax),%gs        UC_GS
 *  15:	b8 a1 01 00 00       	mov    0x1a1,%eax           $SYS_sigreturn
 */

  if (!(   *(unsigned int *)(context->ra - 4) == 0x102454ff
        && *(unsigned int *)(context->ra)     == 0x2024448d ))
    return _URC_END_OF_STACK;

  sf = (struct sigframe *) context->cfa;
  new_cfa = sf->REG_NAME(esp);
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 4;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.how[0] = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sf->REG_NAME(eax) - new_cfa;
  fs->regs.how[3] = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sf->REG_NAME(ebx) - new_cfa;
  fs->regs.how[1] = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sf->REG_NAME(ecx) - new_cfa;
  fs->regs.how[2] = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sf->REG_NAME(edx) - new_cfa;
  fs->regs.how[6] = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sf->REG_NAME(esi) - new_cfa;
  fs->regs.how[7] = REG_SAVED_OFFSET;
  fs->regs.reg[7].loc.offset = (long)&sf->REG_NAME(edi) - new_cfa;
  fs->regs.how[5] = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sf->REG_NAME(ebp) - new_cfa;
  fs->regs.how[8] = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sf->REG_NAME(eip) - new_cfa;
  fs->retaddr_column = 8;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}
#endif /* ifdef __x86_64__  */
