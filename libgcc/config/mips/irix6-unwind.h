/* DWARF2 EH unwinding support for MIPS IRIX 6.
   Copyright (C) 2011 Free Software Foundation, Inc.

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
   state data appropriately.  See unwind-dw2.c for the structs.  */

/* This code was developed-for and only tested-in limited ABI
   configurations.  Characterize that.  */

#if defined (_ABIN32) || defined (_ABI64)
#define SUPPORTED_ABI 1
#else
#define SUPPORTED_ABI 0
#endif

#include <signal.h>

#define MD_FALLBACK_FRAME_STATE_FOR mips_fallback_frame_state

/* Look at the code around RA to see if it matches a sighandler caller with a
   sigcontext_t * argument (SA_SIGINFO cleared).  Return that pointer argument
   if it does match, or 0 otherwise.  */

static sigcontext_t *
sigcontext_for (unsigned int *ra, void *cfa)
{
  /* IRIX 6.5, mono-threaded application.  We're lucky enough to be able
     to expect a short very sighandler specific sequence around.

     <_sigtramp+124>:	li	v0,1088 (SYS_sigreturn)
     <_sigtramp+128>:	syscall  */

  if (   ra[6] == 0x24020440
      && ra[7] == 0x0000000c)
    return (sigcontext_t *)(cfa + 0x30);

  /* IRIX 6.5 variants, multi-threaded application, pthreads.  Nothing really
     sighandler specific handy, so match a fairly long constant sequence.  */

#if _MIPS_SIM == _ABIN32
  /* 
     <sig_fixup_mask+40>:	sd	s0,0(sp)
     <sig_fixup_mask+44>:	sll	ra,a0,0x2
     <sig_fixup_mask+48>:	addiu	t9,t9,-28584/-28456/-28448
     <sig_fixup_mask+52>:	lw	s0,3804(at)
     <sig_fixup_mask+56>:	addu	t9,t9,ra
     <sig_fixup_mask+60>:	lw	t9,0(t9)
     <sig_fixup_mask+64>:	ld	at,3696(at)
     <sig_fixup_mask+68>:	ld	s2,88(s0)
     <sig_fixup_mask+72>:	jalr	t9
     <sig_fixup_mask+76>:	sd	at,88(s0)  */
   if (   ra[-10] == 0xffb00000
      && ra[ -9] == 0x0004f880
      && (ra[-8] == 0x27399058
	  || ra[-8] == 0x273990d8
	  || ra[-8] == 0x273990e0)
      && ra[ -7] == 0x8c300edc
      && ra[ -6] == 0x033fc821
      && ra[ -5] == 0x8f390000
      && ra[ -4] == 0xdc210e70
      && ra[ -3] == 0xde120058
      && ra[ -2] == 0x0320f809
      && ra[ -1] == 0xfe010058)

#elif _MIPS_SIM == _ABI64
  /* 
     <sig_fixup_mask+44>:	sd	s0,0(sp)
     <sig_fixup_mask+48>:	daddu	t9,t9,ra
     <sig_fixup_mask+52>:	dsll	ra,a0,0x3
     <sig_fixup_mask+56>:	ld	s0,3880(at)
     <sig_fixup_mask+60>:	daddu	t9,t9,ra
     <sig_fixup_mask+64>:	ld	t9,0(t9)
     <sig_fixup_mask+68>:	ld	at,3696(at)
     <sig_fixup_mask+72>:	ld	s2,152(s0)
     <sig_fixup_mask+76>:	jalr	t9
     <sig_fixup_mask+80>:	sd	at,152(s0)  */
  if (   ra[-10] == 0xffb00000
      && ra[ -9] == 0x033fc82d
      && ra[ -8] == 0x0004f8f8
      && ra[ -7] == 0xdc300f28
      && ra[ -6] == 0x033fc82d
      && ra[ -5] == 0xdf390000
      && ra[ -4] == 0xdc210e70
      && ra[ -3] == 0xde120098
      && ra[ -2] == 0x0320f809
      && ra[ -1] == 0xfe010098)
#endif
    return (sigcontext_t *)(cfa + 0x60);

  return 0;
}

#define SIGCTX_GREG_ADDR(REGNO,SIGCTX) \
  ((void *) &(SIGCTX)->sc_regs[REGNO])

#define SIGCTX_FPREG_ADDR(REGNO,SIGCTX) \
  ((void *) &(SIGCTX)->sc_fpregs[REGNO])

static _Unwind_Reason_Code
mips_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  /* Return address and CFA of the frame we're attempting to unwind through,
     possibly a signal handler.  */
  void *ctx_ra  = (void *)context->ra;
  void *ctx_cfa = (void *)context->cfa;

  /* CFA of the intermediate abstract kernel frame between the interrupted
     code and the signal handler, if we're indeed unwinding through a signal
     handler.  */
  void *k_cfa;

  /* Pointer to the sigcontext_t structure pushed by the kernel when we're
     unwinding through a signal handler setup with SA_SIGINFO cleared.  */
  sigcontext_t *sigctx;
  int i;

  if (! SUPPORTED_ABI)
    return _URC_END_OF_STACK;
    
  sigctx = sigcontext_for (ctx_ra, ctx_cfa);

  if (sigctx == 0)
    return _URC_END_OF_STACK;

  /* The abstract kernel frame's CFA is extactly the stack pointer
     value at the interruption point.  */
  k_cfa = *(void **)SIGCTX_GREG_ADDR (CTX_SP, sigctx);

  /* State the rules to compute the CFA we have the value of: use the
     previous CFA and offset by the difference between the two.  See
     uw_update_context_1 for the supporting details.  */
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __builtin_dwarf_sp_column ();
  fs->regs.cfa_offset = k_cfa - ctx_cfa;

  /* Fill the internal frame_state structure with information stating where
     each register of interest can be found from the CFA.  */
  for (i = 0; i <= 31; i ++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset = SIGCTX_GREG_ADDR (i, sigctx) - k_cfa;
    }

  for (i = 0; i <= 31; i ++)
    {
      fs->regs.reg[32+i].how = REG_SAVED_OFFSET;
      fs->regs.reg[32+i].loc.offset = SIGCTX_FPREG_ADDR (i, sigctx) - k_cfa;
    }

  /* State the rules to find the kernel's code "return address", which is the
     address of the active instruction when the signal was caught.  */
  fs->retaddr_column = DWARF_FRAME_RETURN_COLUMN;
  fs->regs.reg[fs->retaddr_column].how = REG_SAVED_OFFSET;
  fs->regs.reg[fs->retaddr_column].loc.offset = (void *)&sigctx->sc_pc - k_cfa;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}
