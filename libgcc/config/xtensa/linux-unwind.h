/* DWARF2 EH unwinding support for Xtensa.
   Copyright (C) 2008-2015 Free Software Foundation, Inc.

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
   state data appropriately.  See unwind-dw2-xtensa.c for the structs.
   Don't use this at all if inhibit_libc is used.  */

#ifndef inhibit_libc

#include <signal.h>
#include <sys/ucontext.h>

/* Encoded bytes for Xtensa instructions:
	movi a2, __NR_rt_sigreturn
	syscall
	entry (first byte only)
   Some of the bytes are endian-dependent.  */

#define MOVI_BYTE0 0x22
#define MOVI_BYTE2 225 /* __NR_rt_sigreturn */
#define SYSC_BYTE0 0
#define SYSC_BYTE2 0

#ifdef __XTENSA_EB__
#define MOVI_BYTE1 0x0a
#define SYSC_BYTE1 0x05
#define ENTRY_BYTE 0x6c
#else
#define MOVI_BYTE1 0xa0
#define SYSC_BYTE1 0x50
#define ENTRY_BYTE 0x36
#endif

#define MD_FALLBACK_FRAME_STATE_FOR xtensa_fallback_frame_state

static _Unwind_Reason_Code
xtensa_fallback_frame_state (struct _Unwind_Context *context,
			     _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  struct sigcontext *sc;
#if defined(__XTENSA_CALL0_ABI__)
  _Unwind_Ptr new_cfa;
  int i;
#endif

  struct rt_sigframe {
    siginfo_t info;
    ucontext_t uc;
  } *rt_;

  /* movi a2, __NR_rt_sigreturn; syscall */
  if (pc[0] != MOVI_BYTE0
      || pc[1] != MOVI_BYTE1
      || pc[2] != MOVI_BYTE2
      || pc[3] != SYSC_BYTE0
      || pc[4] != SYSC_BYTE1
      || pc[5] != SYSC_BYTE2)
    return _URC_END_OF_STACK;

#if defined(__XTENSA_WINDOWED_ABI__)
  rt_ = context->sp;
  sc = &rt_->uc.uc_mcontext;
  fs->signal_regs = (_Unwind_Word *) sc->sc_a;

  /* If the signal arrived just before an ENTRY instruction, find the return
     address and pretend the signal arrived before executing the CALL.  */
  if (*(unsigned char *) sc->sc_pc == ENTRY_BYTE)
   {
     unsigned callinc = (sc->sc_ps >> 16) & 3;
     fs->signal_ra = ((sc->sc_a[callinc << 2] & XTENSA_RA_FIELD_MASK)
		      | context->ra_high_bits) - 3;
   }
  else
    fs->signal_ra = sc->sc_pc;
#elif defined(__XTENSA_CALL0_ABI__)
  rt_ = context->cfa;
  sc = &rt_->uc.uc_mcontext;

  new_cfa = (_Unwind_Ptr) sc;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  for (i = 0; i < 16; i++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset = (_Unwind_Ptr) &(sc->sc_a[i]) - new_cfa;
    }

  fs->regs.reg[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__].how =
    REG_SAVED_VAL_OFFSET;
  fs->regs.reg[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__].loc.offset =
    (_Unwind_Ptr) (sc->sc_pc) - new_cfa;
  fs->retaddr_column = __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__;
#else
#error Unsupported Xtensa ABI
#endif

  fs->signal_frame = 1;
  return _URC_NO_REASON;
}


#endif /* ifdef inhibit_libc  */
