/* DWARF2 EH unwinding support for GNU Hurd: x86.
   Copyright (C) 2020-2023 Free Software Foundation, Inc.
   Contributed by Samuel Thibault <samuel.thibault@gnu.org>

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

#ifndef inhibit_libc

#include <signal.h>

#ifdef __x86_64__

/*
 * TODO: support for 64 bits needs to be implemented.
 */

#else /* ifdef __x86_64__  */

#define MD_FALLBACK_FRAME_STATE_FOR x86_gnu_fallback_frame_state

static _Unwind_Reason_Code
x86_gnu_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct handler_args {
    int signo;
    union
      {
	struct
	  {
	    long int sigcode;
	    struct sigcontext *scp;
	  } legacy;
	struct
	  {
	    siginfo_t *siginfop;
	    ucontext_t *uctxp;
	  } posix;
      };
  } *handler_args;
  long int sigcode;
  unsigned long usp;

/*
 * i386 sigtramp frame we are looking for follows.
 * (see glibc/sysdeps/mach/hurd/i386/trampoline.c assembly)
 *
 * rpc_wait_trampoline:
 *   0:	b8 e7 ff ff ff       	mov    $-25,%eax       mach_msg_trap
 *   5:	9a 00 00 00 00 07 00 	lcall  $7,$0
 *  12:	89 01                	movl   %eax, (%ecx)
 *  14:	89 dc                	movl   %ebx, %esp      switch to signal stack
 *
 * trampoline:
 *  16:	ff d2                	call   *%edx           call the handler function
 * RA HERE
 *  18:	83 c4 0c             	addl   $12, %esp       pop its args
 *  21:	c3                   	ret                    return to sigreturn
 *
 * firewall:
 *  22:	f4                   	hlt
 */

  if (!(   *(unsigned int   *)(context->ra     ) == 0xc30cc483
        && *(unsigned char  *)(context->ra +  4) ==       0xf4

        && *(unsigned int   *)(context->ra -  4) == 0xd2ffdc89
        && *(unsigned int   *)(context->ra -  8) == 0x01890007
        && *(unsigned int   *)(context->ra - 12) == 0x00000000
        && *(unsigned int   *)(context->ra - 16) == 0x9affffff
        && *(unsigned short *)(context->ra - 18) ==     0xe7b8))
    return _URC_END_OF_STACK;

  handler_args = context->cfa;
  sigcode = handler_args->legacy.sigcode;
  if (sigcode >= -16 && sigcode < 4096)
    {
      /* This cannot be a SIGINFO pointer, assume legacy.  */
      struct sigcontext *scp = handler_args->legacy.scp;
      usp = scp->sc_uesp;

      fs->regs.reg[0].loc.offset = (unsigned long)&scp->sc_eax - usp;
      fs->regs.reg[1].loc.offset = (unsigned long)&scp->sc_ecx - usp;
      fs->regs.reg[2].loc.offset = (unsigned long)&scp->sc_edx - usp;
      fs->regs.reg[3].loc.offset = (unsigned long)&scp->sc_ebx - usp;
      fs->regs.reg[5].loc.offset = (unsigned long)&scp->sc_ebp - usp;
      fs->regs.reg[6].loc.offset = (unsigned long)&scp->sc_esi - usp;
      fs->regs.reg[7].loc.offset = (unsigned long)&scp->sc_edi - usp;
      fs->regs.reg[8].loc.offset = (unsigned long)&scp->sc_eip - usp;
    }
  else
    {
      /* This is not a valid sigcode, assume SIGINFO.  */
      ucontext_t *uctxp = handler_args->posix.uctxp;
      gregset_t *gregset = &uctxp->uc_mcontext.gregs;
      usp = (*gregset)[REG_UESP];

      fs->regs.reg[0].loc.offset = (unsigned long)&(*gregset)[REG_EAX] - usp;
      fs->regs.reg[1].loc.offset = (unsigned long)&(*gregset)[REG_ECX] - usp;
      fs->regs.reg[2].loc.offset = (unsigned long)&(*gregset)[REG_EDX] - usp;
      fs->regs.reg[3].loc.offset = (unsigned long)&(*gregset)[REG_EBX] - usp;
      fs->regs.reg[5].loc.offset = (unsigned long)&(*gregset)[REG_EBP] - usp;
      fs->regs.reg[6].loc.offset = (unsigned long)&(*gregset)[REG_ESI] - usp;
      fs->regs.reg[7].loc.offset = (unsigned long)&(*gregset)[REG_EDI] - usp;
      fs->regs.reg[8].loc.offset = (unsigned long)&(*gregset)[REG_EIP] - usp;
    }

  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 4;
  fs->regs.cfa_offset = usp - (unsigned long) context->cfa;

  fs->regs.how[0] = REG_SAVED_OFFSET;
  fs->regs.how[1] = REG_SAVED_OFFSET;
  fs->regs.how[2] = REG_SAVED_OFFSET;
  fs->regs.how[3] = REG_SAVED_OFFSET;
  fs->regs.how[5] = REG_SAVED_OFFSET;
  fs->regs.how[6] = REG_SAVED_OFFSET;
  fs->regs.how[7] = REG_SAVED_OFFSET;
  fs->regs.how[8] = REG_SAVED_OFFSET;

  fs->retaddr_column = 8;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#endif /* ifdef __x86_64__  */

#endif /* ifndef inhibit_libc */
