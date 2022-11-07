/* DWARF2 EH unwinding support for PowerPC and PowerPC64 Linux.
   Copyright (C) 2004-2022 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#define R_LR		65
#define R_CR2		70
#define R_CR3		71
#define R_CR4		72
#define R_VR0		77
#define R_VRSAVE	109

#ifdef __powerpc64__
#if _CALL_ELF == 2
#define TOC_SAVE_SLOT	24
#else
#define TOC_SAVE_SLOT	40
#endif
#endif

struct gcc_vregs
{
  __attribute__ ((vector_size (16))) int vr[32];
#ifdef __powerpc64__
  unsigned int pad1[3];
  unsigned int vscr;
  unsigned int vsave;
  unsigned int pad2[3];
#else
  unsigned int vsave;
  unsigned int pad[2];
  unsigned int vscr;
#endif
};

struct gcc_regs
{
  unsigned long gpr[32];
  unsigned long nip;
  unsigned long msr;
  unsigned long orig_gpr3;
  unsigned long ctr;
  unsigned long link;
  unsigned long xer;
  unsigned long ccr;
  unsigned long softe;
  unsigned long trap;
  unsigned long dar;
  unsigned long dsisr;
  unsigned long result;
  unsigned long pad1[4];
  double fpr[32];
  unsigned int pad2;
  unsigned int fpscr;
#ifdef __powerpc64__
  struct gcc_vregs *vp;
#else
  unsigned int pad3[2];
#endif
  struct gcc_vregs vregs;
};

struct gcc_ucontext
{
#ifdef __powerpc64__
  unsigned long pad[28];
#else
  unsigned long pad[12];
#endif
  struct gcc_regs *regs;
  struct gcc_regs rsave;
};

#ifdef __powerpc64__

enum { SIGNAL_FRAMESIZE = 128 };

struct rt_sigframe {
  char gap[SIGNAL_FRAMESIZE];
  struct gcc_ucontext uc;
  unsigned long pad[2];
  int tramp[6];
  void *pinfo;
  struct gcc_ucontext *puc;
};

/* If PC is at a sigreturn trampoline, return a pointer to the
   regs.  Otherwise return NULL.  */

static struct gcc_regs *
get_regs (struct _Unwind_Context *context)
{
  const unsigned int *pc = context->ra;

  /* addi r1, r1, 128; li r0, 0x0077; sc  (sigreturn) */
  /* addi r1, r1, 128; li r0, 0x00AC; sc  (rt_sigreturn) */
  if (pc[0] != 0x38210000 + SIGNAL_FRAMESIZE || pc[2] != 0x44000002)
    return NULL;
  if (pc[1] == 0x38000077)
    {
      struct sigframe {
	char gap[SIGNAL_FRAMESIZE];
	unsigned long pad[7];
	struct gcc_regs *regs;
      } *frame = (struct sigframe *) context->cfa;
      return frame->regs;
    }
  else if (pc[1] == 0x380000AC)
    {
#if _CALL_ELF != 2
      /* These old kernel versions never supported ELFv2.  */
      /* This works for 2.4 kernels, but not for 2.6 kernels with vdso
	 because pc isn't pointing into the stack.  Can be removed when
	 no one is running 2.4.19 or 2.4.20, the first two ppc64
	 kernels released.  */
      const struct rt_sigframe_24 {
	int tramp[6];
	void *pinfo;
	struct gcc_ucontext *puc;
      } *frame24 = (const struct rt_sigframe_24 *) context->ra;

      /* Test for magic value in *puc of vdso.  */
      if ((long) frame24->puc != -21 * 8)
	return frame24->puc->regs;
      else
#endif
	{
	  /* This works for 2.4.21 and later kernels.  */
	  struct rt_sigframe *frame = (struct rt_sigframe *) context->cfa;
	  return frame->uc.regs;
	}
    }
  return NULL;
}

#else  /* !__powerpc64__ */

enum { SIGNAL_FRAMESIZE = 64 };

struct rt_sigframe {
  char gap[SIGNAL_FRAMESIZE + 16];
  char siginfo[128];
  struct gcc_ucontext uc;
};

static struct gcc_regs *
get_regs (struct _Unwind_Context *context)
{
  const unsigned int *pc = context->ra;

  /* li r0, 0x7777; sc  (sigreturn old)  */
  /* li r0, 0x0077; sc  (sigreturn new)  */
  /* li r0, 0x6666; sc  (rt_sigreturn old)  */
  /* li r0, 0x00AC; sc  (rt_sigreturn new)  */
  if (pc[1] != 0x44000002)
    return NULL;
  if (pc[0] == 0x38007777 || pc[0] == 0x38000077)
    {
      struct sigframe {
	char gap[SIGNAL_FRAMESIZE];
	unsigned long pad[7];
	struct gcc_regs *regs;
      } *frame = (struct sigframe *) context->cfa;
      return frame->regs;
    }
  else if (pc[0] == 0x38006666 || pc[0] == 0x380000AC)
    {
      struct rt_sigframe *frame = (struct rt_sigframe *) context->cfa;
      return frame->uc.regs;
    }
  return NULL;
}
#endif

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#define MD_FALLBACK_FRAME_STATE_FOR ppc_fallback_frame_state

static _Unwind_Reason_Code
ppc_fallback_frame_state (struct _Unwind_Context *context,
			  _Unwind_FrameState *fs)
{
  struct gcc_regs *regs = get_regs (context);
  struct gcc_vregs *vregs;
  long cr_offset;
  long new_cfa;
  int i;

  if (regs == NULL)
    return _URC_NORMAL_STOP;

  new_cfa = regs->gpr[__LIBGCC_STACK_POINTER_REGNUM__];
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

#ifdef __powerpc64__
  fs->regs.how[2] = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long) &regs->gpr[2] - new_cfa;
#endif
  for (i = 14; i < 32; i++)
    {
      fs->regs.how[i] = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset = (long) &regs->gpr[i] - new_cfa;
    }

  /* The CR is saved in the low 32 bits of regs->ccr.  */
  cr_offset = (long) &regs->ccr - new_cfa;
#ifndef __LITTLE_ENDIAN__
  cr_offset += sizeof (long) - 4;
#endif
  /* In the ELFv1 ABI, CR2 stands in for the whole CR.  */
  fs->regs.how[R_CR2] = REG_SAVED_OFFSET;
  fs->regs.reg[R_CR2].loc.offset = cr_offset;
#if _CALL_ELF == 2
  /* In the ELFv2 ABI, every CR field has a separate CFI entry.  */
  fs->regs.how[R_CR3] = REG_SAVED_OFFSET;
  fs->regs.reg[R_CR3].loc.offset = cr_offset;
  fs->regs.how[R_CR4] = REG_SAVED_OFFSET;
  fs->regs.reg[R_CR4].loc.offset = cr_offset;
#endif

  fs->regs.how[R_LR] = REG_SAVED_OFFSET;
  fs->regs.reg[R_LR].loc.offset = (long) &regs->link - new_cfa;

  fs->regs.how[ARG_POINTER_REGNUM] = REG_SAVED_OFFSET;
  fs->regs.reg[ARG_POINTER_REGNUM].loc.offset = (long) &regs->nip - new_cfa;
  fs->retaddr_column = ARG_POINTER_REGNUM;
  fs->signal_frame = 1;

  /* If we have a FPU...  */
  for (i = 14; i < 32; i++)
    {
      fs->regs.how[i + 32] = REG_SAVED_OFFSET;
      fs->regs.reg[i + 32].loc.offset = (long) &regs->fpr[i] - new_cfa;
    }

  /* If we have a VMX unit...  */
#ifdef __powerpc64__
  vregs = regs->vp;
#else
  vregs = &regs->vregs;
#endif
  if (regs->msr & (1 << 25))
    {
      for (i = 20; i < 32; i++)
	{
	  fs->regs.how[i + R_VR0] = REG_SAVED_OFFSET;
	  fs->regs.reg[i + R_VR0].loc.offset = (long) &vregs->vr[i] - new_cfa;
	}
    }

  fs->regs.how[R_VRSAVE] = REG_SAVED_OFFSET;
  fs->regs.reg[R_VRSAVE].loc.offset = (long) &vregs->vsave - new_cfa;

  /* If we have SPE register high-parts... we check at compile-time to
     avoid expanding the code for all other PowerPC.  */
#ifdef __SPE__
  for (i = 14; i < 32; i++)
    {
      fs->regs.how[i + FIRST_SPE_HIGH_REGNO - 4] = REG_SAVED_OFFSET;
      fs->regs.reg[i + FIRST_SPE_HIGH_REGNO - 4].loc.offset
	= (long) &regs->vregs - new_cfa + 4 * i;
    }
#endif

  return _URC_NO_REASON;
}

#define MD_FROB_UPDATE_CONTEXT frob_update_context

static void
frob_update_context (struct _Unwind_Context *context, _Unwind_FrameState *fs ATTRIBUTE_UNUSED)
{
  const unsigned int *pc = (const unsigned int *) context->ra;

  /* Fix up for 2.6.12 - 2.6.16 Linux kernels that have vDSO, but don't
     have S flag in it.  */
#ifdef __powerpc64__
  /* addi r1, r1, 128; li r0, 0x0077; sc  (sigreturn) */
  /* addi r1, r1, 128; li r0, 0x00AC; sc  (rt_sigreturn) */
  if (pc[0] == 0x38210000 + SIGNAL_FRAMESIZE
      && (pc[1] == 0x38000077 || pc[1] == 0x380000AC)
      && pc[2] == 0x44000002)
    _Unwind_SetSignalFrame (context, 1);
#else
  /* li r0, 0x7777; sc  (sigreturn old)  */
  /* li r0, 0x0077; sc  (sigreturn new)  */
  /* li r0, 0x6666; sc  (rt_sigreturn old)  */
  /* li r0, 0x00AC; sc  (rt_sigreturn new)  */
  if ((pc[0] == 0x38007777 || pc[0] == 0x38000077
       || pc[0] == 0x38006666 || pc[0] == 0x380000AC)
      && pc[1] == 0x44000002)
    _Unwind_SetSignalFrame (context, 1);
#endif

#ifdef __powerpc64__
  if (fs->regs.how[2] == REG_UNSAVED)
    {
      /* If the current unwind info (FS) does not contain explicit info
	 saving R2, then we have to do a minor amount of code reading to
	 figure out if it was saved.  The big problem here is that the
	 code that does the save/restore is generated by the linker, so
	 we have no good way to determine at compile time what to do.  */
      if (pc[0] == 0xF8410000 + TOC_SAVE_SLOT
#if _CALL_ELF != 2
	  /* The ELFv2 linker never generates the old PLT stub form.  */
	  || ((pc[0] & 0xFFFF0000) == 0x3D820000
	      && pc[1] == 0xF8410000 + TOC_SAVE_SLOT)
#endif
	  )
	{
	  /* We are in a plt call stub or r2 adjusting long branch stub,
	     before r2 has been saved.  Keep REG_UNSAVED.  */
	}
      else
	{
	  unsigned int *insn
	    = (unsigned int *) _Unwind_GetGR (context, R_LR);
	  if (insn && *insn == 0xE8410000 + TOC_SAVE_SLOT)
	    _Unwind_SetGRPtr (context, 2, context->cfa + TOC_SAVE_SLOT);
#if _CALL_ELF != 2
	  /* ELFv2 does not use this function pointer call sequence.  */
	  else if (pc[0] == 0x4E800421
		   && pc[1] == 0xE8410000 + TOC_SAVE_SLOT)
	    {
	      /* We are at the bctrl instruction in a call via function
		 pointer.  gcc always emits the load of the new R2 just
		 before the bctrl so this is the first and only place
		 we need to use the stored R2.  */
	      _Unwind_Word sp = _Unwind_GetGR (context, 1);
	      _Unwind_SetGRPtr (context, 2, (void *)(sp + TOC_SAVE_SLOT));
	    }
#endif
	}
    }
#endif
}

#define MD_BACKCHAIN_FALLBACK ppc_backchain_fallback

struct trace_arg
{
  /* Stores the list of addresses.  */
  void **array;
  struct unwind_link *unwind_link;
  _Unwind_Word cfa;
  /* Number of addresses currently stored.  */
  int count;
  /* Maximum number of addresses.  */
  int size;
};

/* This is the stack layout we see with every stack frame.
   Note that every routine is required by the ABI to lay out the stack
   like this.

	    +----------------+        +-----------------+
    %r1  -> | previous frame--------> | previous frame--->...  --> NULL
	    |                |        |                 |
	    | cr save        |        | cr save	        |
	    |                |        |                 |
	    | (unused)       |        | lr save         |
	    +----------------+        +-----------------+

  The CR save is only present on 64-bit ABIs.
*/
struct frame_layout
{
  struct frame_layout *backchain;
#ifdef __powerpc64__
  long int cr_save;
#endif
  void *lr_save;
};


static void
ppc_backchain_fallback (struct _Unwind_Context *context, void *a)
{
  struct frame_layout *current;
  struct trace_arg *arg = a;
  int count;

  /* Get the last address computed.  */
  current = context->cfa;

  /* If the trace CFA is not the context CFA the backtrace is done.  */
  if (arg == NULL || arg->cfa != current)
	return;

  /* Start with next address.  */
  current = current->backchain;

  for (count = arg->count; current != NULL; current = current->backchain)
    {
      arg->array[count] = current->lr_save;

      /* Check if the symbol is the signal trampoline and get the interrupted
	 symbol address from the trampoline saved area.  */
      context->ra = current->lr_save;
      if (current->lr_save && get_regs (context))
	{
	  struct rt_sigframe *sigframe = (struct rt_sigframe *) current;
	  if (count + 1 == arg->size)
	    break;
	  arg->array[++count] = (void *) sigframe->uc.rsave.nip;
	  current = (void *) sigframe->uc.rsave.gpr[1];
	}
      if (count++ >= arg->size)
	break;
    }

  arg->count = count-1;
}
