/* DWARF2 EH unwinding support for AMD x86-64 and x86.
   Copyright (C) 2004-2022 Free Software Foundation, Inc.

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

/* Unwind shadow stack for -fcf-protection -mshstk.  */
#if defined __SHSTK__ && defined __CET__ && (__CET__ & 2) != 0
# include "config/i386/shadow-stack-unwind.h"
#endif

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.
   Don't use this at all if inhibit_libc is used.  */

#ifndef inhibit_libc

/* There's no sys/ucontext.h for glibc 2.0, so no
   signal-turned-exceptions for them.  There's also no configure-run for
   the target, so we can't check on (e.g.) HAVE_SYS_UCONTEXT_H.  Using the
   target libc version macro should be enough.  */
#if defined __GLIBC__ && !(__GLIBC__ == 2 && __GLIBC_MINOR__ == 0)

#include <signal.h>
#include <sys/ucontext.h>

#ifdef __x86_64__

#define MD_FALLBACK_FRAME_STATE_FOR x86_64_fallback_frame_state

static _Unwind_Reason_Code
x86_64_fallback_frame_state (struct _Unwind_Context *context,
			     _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  struct sigcontext *sc;
  long new_cfa;

  /* movq $__NR_rt_sigreturn, %rax ; syscall.  */
#ifdef __LP64__
#define RT_SIGRETURN_SYSCALL	0x050f0000000fc0c7ULL
#else
#define RT_SIGRETURN_SYSCALL	0x050f40000201c0c7ULL
#endif
  if (*(unsigned char *)(pc+0) == 0x48
      && *(unsigned long long *)(pc+1) == RT_SIGRETURN_SYSCALL)
    {
      ucontext_t *uc_ = context->cfa;
      /* The void * cast is necessary to avoid an aliasing warning.
         The aliasing warning is correct, but should not be a problem
         because it does not alias anything.  */
      sc = (struct sigcontext *) (void *) &uc_->uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = sc->rsp;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  /* Register 7 is rsp  */
  fs->regs.cfa_reg = 7;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.how[0] = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sc->rax - new_cfa;
  fs->regs.how[1] = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sc->rdx - new_cfa;
  fs->regs.how[2] = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sc->rcx - new_cfa;
  fs->regs.how[3] = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sc->rbx - new_cfa;
  fs->regs.how[4] = REG_SAVED_OFFSET;
  fs->regs.reg[4].loc.offset = (long)&sc->rsi - new_cfa;
  fs->regs.how[5] = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sc->rdi - new_cfa;
  fs->regs.how[6] = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sc->rbp - new_cfa;
  fs->regs.how[8] = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sc->r8 - new_cfa;
  fs->regs.how[9] = REG_SAVED_OFFSET;
  fs->regs.reg[9].loc.offset = (long)&sc->r9 - new_cfa;
  fs->regs.how[10] = REG_SAVED_OFFSET;
  fs->regs.reg[10].loc.offset = (long)&sc->r10 - new_cfa;
  fs->regs.how[11] = REG_SAVED_OFFSET;
  fs->regs.reg[11].loc.offset = (long)&sc->r11 - new_cfa;
  fs->regs.how[12] = REG_SAVED_OFFSET;
  fs->regs.reg[12].loc.offset = (long)&sc->r12 - new_cfa;
  fs->regs.how[13] = REG_SAVED_OFFSET;
  fs->regs.reg[13].loc.offset = (long)&sc->r13 - new_cfa;
  fs->regs.how[14] = REG_SAVED_OFFSET;
  fs->regs.reg[14].loc.offset = (long)&sc->r14 - new_cfa;
  fs->regs.how[15] = REG_SAVED_OFFSET;
  fs->regs.reg[15].loc.offset = (long)&sc->r15 - new_cfa;
  fs->regs.how[16] = REG_SAVED_OFFSET;
  fs->regs.reg[16].loc.offset = (long)&sc->rip - new_cfa;
  fs->retaddr_column = 16;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}

#else /* ifdef __x86_64__  */

#define MD_FALLBACK_FRAME_STATE_FOR x86_fallback_frame_state

static _Unwind_Reason_Code
x86_fallback_frame_state (struct _Unwind_Context *context,
			  _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  struct sigcontext *sc;
  long new_cfa;

  /* popl %eax ; movl $__NR_sigreturn,%eax ; int $0x80  */
  if (*(unsigned short *)(pc+0) == 0xb858
      && *(unsigned int *)(pc+2) == 119
      && *(unsigned short *)(pc+6) == 0x80cd)
    sc = context->cfa + 4;
  /* movl $__NR_rt_sigreturn,%eax ; int $0x80  */
  else if (*(unsigned char *)(pc+0) == 0xb8
	   && *(unsigned int *)(pc+1) == 173
	   && *(unsigned short *)(pc+5) == 0x80cd)
    {
      struct rt_sigframe {
	int sig;
	siginfo_t *pinfo;
	void *puc;
	siginfo_t info;
	ucontext_t uc;
      } *rt_ = context->cfa;
      /* The void * cast is necessary to avoid an aliasing warning.
         The aliasing warning is correct, but should not be a problem
         because it does not alias anything.  */
      sc = (struct sigcontext *) (void *) &rt_->uc.uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = sc->esp;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 4;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.how[0] = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sc->eax - new_cfa;
  fs->regs.how[3] = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sc->ebx - new_cfa;
  fs->regs.how[1] = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sc->ecx - new_cfa;
  fs->regs.how[2] = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sc->edx - new_cfa;
  fs->regs.how[6] = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sc->esi - new_cfa;
  fs->regs.how[7] = REG_SAVED_OFFSET;
  fs->regs.reg[7].loc.offset = (long)&sc->edi - new_cfa;
  fs->regs.how[5] = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sc->ebp - new_cfa;
  fs->regs.how[8] = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sc->eip - new_cfa;
  fs->retaddr_column = 8;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}

#define MD_FROB_UPDATE_CONTEXT x86_frob_update_context

/* Fix up for kernels that have vDSO, but don't have S flag in it.  */

static void
x86_frob_update_context (struct _Unwind_Context *context,
			 _Unwind_FrameState *fs ATTRIBUTE_UNUSED)
{
  unsigned char *pc = context->ra;

  /* movl $__NR_rt_sigreturn,%eax ; {int $0x80 | syscall}  */
  if (*(unsigned char *)(pc+0) == 0xb8
      && *(unsigned int *)(pc+1) == 173
      && (*(unsigned short *)(pc+5) == 0x80cd
	  || *(unsigned short *)(pc+5) == 0x050f))
    _Unwind_SetSignalFrame (context, 1);
}

#endif /* ifdef __x86_64__  */
#endif /* not glibc 2.0 */
#endif /* ifdef inhibit_libc  */
