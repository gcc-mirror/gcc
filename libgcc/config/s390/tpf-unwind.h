/* DWARF2 EH unwinding support for TPF OS.
   Copyright (C) 2004-2021 Free Software Foundation, Inc.
   Contributed by P.J. Darcy (darcypj@us.ibm.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include <dlfcn.h>
#include <stdbool.h>

/* Function Name: __isPATrange
   Parameters passed into it:  address to check
   Return Value: A 1 if address is in pat code "range", 0 if not
   Description: This function simply checks to see if the address
   passed to it is in the CP pat code range.  */

#define CP_CNF  0x000000000000c18u /* location of BSS CINFC pointer */
#define cinfc_fast(TAG) (void *) \
  *((unsigned long *) *(unsigned long *) (CP_CNF) + (TAG))
#define CINFC_CMRESET 187
#define CINTFC_CMCENBKST 431
#define CINTFC_CMCENBKED 432

static inline unsigned int
__isPATrange (void *addr)
{
  return !!(addr > cinfc_fast (CINTFC_CMCENBKST)
	    && addr < cinfc_fast (CINTFC_CMCENBKED));
}

static inline unsigned int
__isSkipResetAddr (void *addr)
{
  return !!(addr == cinfc_fast (CINFC_CMRESET));
}

/* TPF return address offset from start of stack frame.  */
#define ICST_CRET 168
#define ICST_SRET 320

/* Exceptions macro defined for TPF so that functions without
   dwarf frame information can be used with exceptions.  */
#define MD_FALLBACK_FRAME_STATE_FOR s390_fallback_frame_state

static _Unwind_Reason_Code
s390_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  unsigned long int regs;
  unsigned long int new_cfa;
  int i;

  regs = *((unsigned long int *)
        (((unsigned long int) context->cfa) - STACK_POINTER_OFFSET));

  /* Are we going through special linkage code?  */
  if (__isPATrange (context->ra) || __isSkipResetAddr (context->ra))
    {

      /* Our return register isn't zero for end of stack, so
         check backward stackpointer to see if it is zero.  */
      if (regs == 0)
         return _URC_END_OF_STACK;

      /* No stack frame.  */
      fs->regs.cfa_how = CFA_REG_OFFSET;
      fs->regs.cfa_reg = 15;
      fs->regs.cfa_offset = STACK_POINTER_OFFSET;

      /* All registers remain unchanged ...  */
      for (i = 0; i < 32; i++)
	{
	  fs->regs.reg[i].how = REG_SAVED_REG;
	  fs->regs.reg[i].loc.reg = i;
	}

      /* ... except for %r14, which is stored at CFA+offset where offset
	 is displacment of ICST_CRET or ICST_SRET from CFA */
      if ( __isPATrange(context->ra) )  {
	   fs->regs.reg[14].how = REG_SAVED_OFFSET;
	   fs->regs.reg[14].loc.offset = ICST_CRET - STACK_POINTER_OFFSET;
	   fs->retaddr_column = 14;
      }  else  {
	   fs->regs.reg[14].how = REG_SAVED_OFFSET;
	   fs->regs.reg[14].loc.offset = ICST_SRET - STACK_POINTER_OFFSET;
	   fs->retaddr_column = 14;

      }

      return _URC_NO_REASON;
    }

  regs = *((unsigned long int *)
        (((unsigned long int) context->cfa) - STACK_POINTER_OFFSET));
  new_cfa = regs + STACK_POINTER_OFFSET;

  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 15;
  fs->regs.cfa_offset = new_cfa -
        (unsigned long int) context->cfa + STACK_POINTER_OFFSET;

  for (i = 0; i < 16; i++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset = regs + i*8 - new_cfa;
    }

  for (i = 0; i < 4; i++)
    {
      fs->regs.reg[16 + i].how = REG_SAVED_OFFSET;
      fs->regs.reg[16 + i].loc.offset = regs + 16*8 + i*8 - new_cfa;
    }

  fs->retaddr_column = 14;

  return _URC_NO_REASON;
}

/* Function Name: __tpf_eh_return
   Parameters passed into it: Destination address to jump to.
   Return Value: Converted Destination address if a Pat Stub exists.
   Description: This function swaps the unwinding return address
      with the cp stub code.  The original target return address is
      then stored into the tpf return address field.  The cp stub
      code is searched for by climbing back up the stack and
      comparing the tpf stored return address object address to
      that of the targets object address.  */

#define CURRENT_STACK_PTR() \
  ({ register unsigned long int *stack_ptr asm ("%r15"); stack_ptr; })

#define PREVIOUS_STACK_PTR() \
  ((unsigned long int *)(*(CURRENT_STACK_PTR())))

#define RA_OFFSET 112
#define R15_OFFSET 120
#define TPFAREA_OFFSET 160
#define TPFAREA_SIZE STACK_POINTER_OFFSET-TPFAREA_OFFSET
#define INVALID_RETURN 0

#define LOWCORE_PAGE3_ADDR 4032
#define PG3_SKIPPING_OFFSET 18

void * __tpf_eh_return (void *target, void *origRA);

void *
__tpf_eh_return (void *target, void *origRA)
{
  Dl_info targetcodeInfo, currentcodeInfo;
  int retval;
  void *current, *stackptr, *destination_frame;
  unsigned char *skipFlagAddress;
  unsigned long int shifter;
  bool is_a_stub;

  is_a_stub = false;

  /* Get code info for target return's address.  */
  retval = dladdr (target, &targetcodeInfo);

  /* Ensure the code info is valid (for target).  */
  if (retval != INVALID_RETURN)
    {
      /* Begin climbing stack searching for target address. */
      stackptr = (void *) *(CURRENT_STACK_PTR());

      /* Get return address based on our stackptr. */
      current = (void *) *(unsigned long *) (stackptr + RA_OFFSET);

      /* Is current return address our initiating exception stack
	 frame? If not, climb the stack one more frame. */
      if (current != origRA)  {
	   stackptr = (void *) *(unsigned long *) stackptr;
      }

      /* Begin looping through stack frames.  Stop if invalid
         code information is retrieved or if a match between the
         current stack frame iteration shared object's address
         matches that of the target, calculated above.  */
      do
        {
	  /* Get return address based on our stackptr iterator.  */
	  current = (void *) *(unsigned long *) (stackptr + RA_OFFSET);

	  /* Is it a Pat Stub?  */
	  if (__isPATrange (current)
	      || (__isSkipResetAddr (current)
		  && __isPATrange ((void *) *(unsigned long *) (stackptr
								+ ICST_SRET))))
	    {
	      /* Yes it was, get real return address in TPF stack area.  */
	      current = (void *) *(unsigned long *) (stackptr + ICST_CRET);
	      is_a_stub = true;
	    }

          /* Get codeinfo on RA so that we can figure out
             the module address.  */
          retval = dladdr (current, &currentcodeInfo);

          /* Check that codeinfo for current stack frame is valid.
             Then compare the module address of current stack frame
             to target stack frame to determine if we have the pat
             stub address we want.  Also ensure we are dealing
             with a module crossing, stub return address. */
          if (is_a_stub && retval != INVALID_RETURN
             && targetcodeInfo.dli_fbase == currentcodeInfo.dli_fbase)
             {
               /* Yes! They are in the same module.
                  Force copy of TPF private stack area to
                  destination stack frame TPF private area. */
               destination_frame = (void *) *((unsigned long int *)
                   (*PREVIOUS_STACK_PTR() + R15_OFFSET));

               /* Copy TPF linkage area from current frame to
                  destination frame.  */
               memcpy((void *) (destination_frame + TPFAREA_OFFSET),
                 (void *) (stackptr + TPFAREA_OFFSET), TPFAREA_SIZE);

               /* Now overlay the
                  real target address into the TPF stack area of
                  the target frame we are jumping to.  */
	       *(unsigned long *) (destination_frame + ICST_CRET) =
		 (unsigned long) target;

               /* Before returning the desired pat stub address to
                  the exception handling unwinder so that it can
                  actually do the "leap" shift out the low order
                  bit designated to determine if we are in 64BIT mode.
                  This is necessary for CTOA stubs.
                  Otherwise we leap one byte past where we want to
                  go to in the TPF pat stub linkage code.  */
	       shifter = *(unsigned long *) (stackptr + RA_OFFSET);

               shifter &= ~1ul;

               /* Store Pat Stub Address in destination Stack Frame.  */
               *((unsigned long int *) (destination_frame +
                   RA_OFFSET)) = shifter;

               /* Re-adjust pat stub address to go to correct place
                  in linkage.  */
               shifter = shifter - 4;

	       /* Reset the Function Trace Skipping Switch to re-enable */
	       /* recording Trace entries if it was turned off. */
	       skipFlagAddress =
		 (unsigned char *) *(unsigned long *) LOWCORE_PAGE3_ADDR;
	       skipFlagAddress += PG3_SKIPPING_OFFSET;
	       *skipFlagAddress = '\x00';

               return (void *) shifter;
             }

          /* Desired module pat stub not found ...
             Bump stack frame iterator.  */
          stackptr = (void *) *(unsigned long int *) stackptr;

          is_a_stub = false;

        }  while (stackptr && retval != INVALID_RETURN
                && targetcodeInfo.dli_fbase != currentcodeInfo.dli_fbase);
    }

  /* Reset the Function Trace Skipping Switch to re-enable */
  /* recording Trace entries if it was turned off. */
  skipFlagAddress = (unsigned char *) *(unsigned long *) LOWCORE_PAGE3_ADDR;
  skipFlagAddress += PG3_SKIPPING_OFFSET;
  *skipFlagAddress = '\x00';

  /* No pat stub found, could be a problem?  Simply return unmodified
     target address.  */
  return target;
}
