/* -----------------------------------------------------------------------
   ffi.c - Copyright (c) 1998 Geoffrey Keating
   
   PowerPC Foreign Function Interface 

   $Id: ffi.c,v 1.1.1.1 1998/11/29 16:48:16 green Exp $

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   ``Software''), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------- */

#include <ffi.h>
#include <ffi_common.h>

#include <stdlib.h>

enum {
  /* The assembly depends on these exact flags.  */
  FLAG_RETURNS_NOTHING  = 1 << (31-30), /* These go in cr7 */
  FLAG_RETURNS_FP       = 1 << (31-29),
  FLAG_RETURNS_64BITS   = 1 << (31-28),

  FLAG_ARG_NEEDS_COPY   = 1 << (31- 7),
  FLAG_FP_ARGUMENTS     = 1 << (31- 6), /* cr1.eq; specified by ABI */
  FLAG_4_GPR_ARGUMENTS  = 1 << (31- 5),
  FLAG_RETVAL_REFERENCE = 1 << (31- 4)
};

/* About the SYSV ABI.  */
enum {
  NUM_GPR_ARG_REGISTERS = 8,
  NUM_FPR_ARG_REGISTERS = 8
};
enum { ASM_NEEDS_REGISTERS = 4 };

/* ffi_prep_args is called by the assembly routine once stack space
   has been allocated for the function's arguments.

   The stack layout we want looks like this:

   |   Return address from ffi_call_SYSV 4bytes	|	higher addresses
   |--------------------------------------------|
   |   Previous backchain pointer	4	| 	stack pointer here
   |--------------------------------------------|<+ <<<	on entry to
   |   Saved r28-r31			4*4	| |	ffi_call_SYSV
   |--------------------------------------------| |
   |   GPR registers r3-r10		8*4	| |	ffi_call_SYSV
   |--------------------------------------------| |
   |   FPR registers f1-f8 (optional)	8*8	| |
   |--------------------------------------------| |	stack	|
   |   Space for copied structures		| |	grows	|
   |--------------------------------------------| |	down    V
   |   Parameters that didn't fit in registers  | |
   |--------------------------------------------| |	lower addresses
   |   Space for callee's LR		4	| |
   |--------------------------------------------| |	stack pointer here
   |   Current backchain pointer	4	|-/	during
   |--------------------------------------------|   <<<	ffi_call_SYSV

   */

/*@-exportheader@*/
void ffi_prep_args(extended_cif *ecif, unsigned *const stack)
/*@=exportheader@*/
{
  const unsigned bytes = ecif->cif->bytes;
  const unsigned flags = ecif->cif->flags;
  
  /* 'stacktop' points at the previous backchain pointer.  */
  unsigned *const stacktop = stack + (ecif->cif->bytes / sizeof(unsigned));

  /* 'gpr_base' points at the space for gpr3, and grows upwards as
     we use GPR registers.  */
  unsigned *gpr_base = stacktop - ASM_NEEDS_REGISTERS - NUM_GPR_ARG_REGISTERS;
  int intarg_count = 0;

  /* 'fpr_base' points at the space for fpr1, and grows upwards as
     we use FPR registers.  */
  double *fpr_base = (double *)gpr_base - NUM_FPR_ARG_REGISTERS;
  int fparg_count = 0;

  /* 'copy_space' grows down as we put structures in it.  It should
     stay 16-byte aligned.  */
  char *copy_space = ((flags & FLAG_FP_ARGUMENTS)
		      ? (char *)fpr_base
		      : (char *)gpr_base);

  /* 'next_arg' grows up as we put parameters in it.  */
  unsigned *next_arg = stack + 2;

  int i;
  ffi_type **ptr;
  double double_tmp;
  void **p_argv;
  size_t struct_copy_size;
  unsigned gprvalue;

  /* Check that everything starts aligned properly.  */
  FFI_ASSERT(((unsigned)(char *)stack & 0xF) == 0);
  FFI_ASSERT(((unsigned)(char *)copy_space & 0xF) == 0);
  FFI_ASSERT(((unsigned)(char *)stacktop & 0xF) == 0);
  FFI_ASSERT((bytes & 0xF) == 0);
  FFI_ASSERT(copy_space >= (char *)next_arg);

  /* Deal with return values that are actually pass-by-reference.  */
  if (flags & FLAG_RETVAL_REFERENCE)
  {
    *gpr_base++ = (unsigned)(char *)ecif->rvalue;
    intarg_count++;
  }

  /* Now for the arguments.  */
  p_argv = ecif->avalue;
  for (ptr = ecif->cif->arg_types, i = ecif->cif->nargs;
       i > 0;
       i--, ptr++, p_argv++)
    {
      switch ((*ptr)->type)
	{
	case FFI_TYPE_FLOAT:
	case FFI_TYPE_DOUBLE:
	  if ((*ptr)->type == FFI_TYPE_FLOAT)
	    double_tmp = *(float *)*p_argv;
	  else
	    double_tmp = *(double *)*p_argv;

	  if (fparg_count >= NUM_FPR_ARG_REGISTERS)
	    {
	      if (intarg_count%2 != 0)
		{
		  intarg_count++;
		  next_arg++;
		}
	      *(double *)next_arg = double_tmp;
	      next_arg += 2;
	    }
	  else
	    *fpr_base++ = double_tmp;
	  fparg_count++;
	  FFI_ASSERT(flags & FLAG_FP_ARGUMENTS);
	  break;

	case FFI_TYPE_UINT64:
	case FFI_TYPE_SINT64:
	  if (intarg_count == NUM_GPR_ARG_REGISTERS-1)
	    intarg_count++;
	  if (intarg_count >= NUM_GPR_ARG_REGISTERS)
	    {
	      if (intarg_count%2 != 0)
		{
		  intarg_count++;
		  next_arg++;
		}
	      *(long long *)next_arg = *(long long *)*p_argv;
	      next_arg += 2;
	    }
	  else
	    {
	      *(long long *)gpr_base = *(long long *)*p_argv;
	      gpr_base += 2;
	    }
	  intarg_count += 2;
	  break;

	case FFI_TYPE_STRUCT:
#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
	case FFI_TYPE_LONGDOUBLE:
#endif
	  struct_copy_size = ((*ptr)->size + 15) & ~0xF;
	  copy_space -= struct_copy_size;
	  memcpy(copy_space, (char *)*p_argv, (*ptr)->size);
	  
	  gprvalue = (unsigned)copy_space;

	  FFI_ASSERT(copy_space > (char *)next_arg);
	  FFI_ASSERT(flags & FLAG_ARG_NEEDS_COPY);
	  goto putgpr;

	case FFI_TYPE_UINT8:
	  gprvalue = *(unsigned char *)*p_argv;
	  goto putgpr;
	case FFI_TYPE_SINT8:
	  gprvalue = *(signed char *)*p_argv;
	  goto putgpr;
	case FFI_TYPE_UINT16:
	  gprvalue = *(unsigned short *)*p_argv;
	  goto putgpr;
	case FFI_TYPE_SINT16:
	  gprvalue = *(signed short *)*p_argv;
	  goto putgpr;

	case FFI_TYPE_INT:
	case FFI_TYPE_UINT32:
	case FFI_TYPE_SINT32:
	case FFI_TYPE_POINTER:
	  gprvalue = *(unsigned *)*p_argv;
	putgpr:
	  if (intarg_count >= NUM_GPR_ARG_REGISTERS)
	    *next_arg++ = gprvalue;
	  else
	    *gpr_base++ = gprvalue;
	  intarg_count++;
	  break;
	}
    }

  /* Check that we didn't overrun the stack...  */
  FFI_ASSERT(copy_space >= (char *)next_arg);
  FFI_ASSERT(gpr_base <= stacktop - ASM_NEEDS_REGISTERS);
  FFI_ASSERT((unsigned *)fpr_base
	     <= stacktop - ASM_NEEDS_REGISTERS - NUM_GPR_ARG_REGISTERS);
  FFI_ASSERT(flags & FLAG_4_GPR_ARGUMENTS || intarg_count <= 4);
}

/* Perform machine dependent cif processing */
ffi_status ffi_prep_cif_machdep(ffi_cif *cif)
{
  /* All this is for the SYSV ABI.  */
  int i;
  ffi_type **ptr;
  unsigned bytes;
  int fparg_count = 0, intarg_count = 0;
  unsigned flags = 0;
  unsigned struct_copy_size = 0;
  
  /* All the machine-independent calculation of cif->bytes will be wrong.
     Redo the calculation for SYSV.  */

  /* Space for the frame pointer, callee's LR, and the asm's temp regs.  */
  bytes = (2 + ASM_NEEDS_REGISTERS) * sizeof(int);

  /* Space for the GPR registers.  */
  bytes += NUM_GPR_ARG_REGISTERS * sizeof(int);

  /* Return value handling.  The rules are as follows:
     - 32-bit (or less) integer values are returned in gpr3;
     - Structures of size <= 4 bytes also returned in gpr3;
     - 64-bit integer values and structures between 5 and 8 bytes are returned
       in gpr3 and gpr4;
     - Single/double FP values are returned in fpr1;
     - Larger structures and long double (if not equivalent to double) values
       are allocated space and a pointer is passed as the first argument.  */
  switch (cif->rtype->type)
    {
    case FFI_TYPE_DOUBLE:
      flags |= FLAG_RETURNS_64BITS;
      /* Fall through.  */
    case FFI_TYPE_FLOAT:
      flags |= FLAG_RETURNS_FP;
      break;

    case FFI_TYPE_UINT64:
    case FFI_TYPE_SINT64:
      flags |= FLAG_RETURNS_64BITS;
      break;

    case FFI_TYPE_STRUCT:
      if (cif->abi != FFI_GCC_SYSV)
	if (cif->rtype->size <= 4)
	  break;
	else if (cif->rtype->size <= 8)
	  {
	    flags |= FLAG_RETURNS_64BITS;
	    break;
	  }
      /* else fall through.  */
#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
    case FFI_TYPE_LONGDOUBLE:
#endif
      intarg_count++;
      flags |= FLAG_RETVAL_REFERENCE;
      /* Fall through.  */
    case FFI_TYPE_VOID:
      flags |= FLAG_RETURNS_NOTHING;
      break;

    default:
      /* Returns 32-bit integer, or similar.  Nothing to do here.  */
      break;
    }

  /* The first NUM_GPR_ARG_REGISTERS words of integer arguments, and the
     first NUM_FPR_ARG_REGISTERS fp arguments, go in registers; the rest
     goes on the stack.  Structures and long doubles (if not equivalent
     to double) are passed as a pointer to a copy of the structure.
     Stuff on the stack needs to keep proper alignment.  */
  for (ptr = cif->arg_types, i = cif->nargs; i > 0; i--, ptr++)
    {
      switch ((*ptr)->type)
	{
	case FFI_TYPE_FLOAT:
	case FFI_TYPE_DOUBLE:
	  fparg_count++;
	  /* If this FP arg is going on the stack, it must be
	     8-byte-aligned.  */
	  if (fparg_count > NUM_FPR_ARG_REGISTERS
	      && intarg_count%2 != 0)
	    intarg_count++;
	  break;

	case FFI_TYPE_UINT64:
	case FFI_TYPE_SINT64:
	  /* 'long long' arguments are passed as two words, but
	     either both words must fit in registers or both go
	     on the stack.  If they go on the stack, they must
	     be 8-byte-aligned.  */
	  if (intarg_count == NUM_GPR_ARG_REGISTERS-1
	      || intarg_count >= NUM_GPR_ARG_REGISTERS && intarg_count%2 != 0)
	    intarg_count++;
	  intarg_count += 2;
	  break;

	case FFI_TYPE_STRUCT:
#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
	case FFI_TYPE_LONGDOUBLE:
#endif
	  /* We must allocate space for a copy of these to enforce
	     pass-by-value.  Pad the space up to a multiple of 16
	     bytes (the maximum alignment required for anything under
	     the SYSV ABI).  */
	  struct_copy_size += ((*ptr)->size + 15) & ~0xF;
	  /* Fall through (allocate space for the pointer).  */

	default:
	  /* Everything else is passed as a 4-byte word in a GPR, either
	     the object itself or a pointer to it.  */
	  intarg_count++;
	  break;
	}
    }

  if (fparg_count != 0)
    flags |= FLAG_FP_ARGUMENTS;
  if (intarg_count > 4)
    flags |= FLAG_4_GPR_ARGUMENTS;
  if (struct_copy_size != 0)
    flags |= FLAG_ARG_NEEDS_COPY;
  
  /* Space for the FPR registers, if needed.  */
  if (fparg_count != 0)
    bytes += NUM_FPR_ARG_REGISTERS * sizeof(double);

  /* Stack space.  */
  if (intarg_count > NUM_GPR_ARG_REGISTERS)
    bytes += (intarg_count - NUM_GPR_ARG_REGISTERS) * sizeof(int);
  if (fparg_count > NUM_FPR_ARG_REGISTERS)
    bytes += (fparg_count - NUM_FPR_ARG_REGISTERS) * sizeof(double);

  /* The stack space allocated needs to be a multiple of 16 bytes.  */
  bytes = (bytes + 15) & ~0xF;

  /* Add in the space for the copied structures.  */
  bytes += struct_copy_size;

  cif->flags = flags;
  cif->bytes = bytes;

  return FFI_OK;
}

/*@-declundef@*/
/*@-exportheader@*/
extern void ffi_call_SYSV(/*@out@*/ extended_cif *, 
			  unsigned, unsigned, 
			  /*@out@*/ unsigned *, 
			  void (*fn)());
/*@=declundef@*/
/*@=exportheader@*/

void ffi_call(/*@dependent@*/ ffi_cif *cif, 
	      void (*fn)(), 
	      /*@out@*/ void *rvalue, 
	      /*@dependent@*/ void **avalue)
{
  extended_cif ecif;

  ecif.cif = cif;
  ecif.avalue = avalue;
  
  /* If the return value is a struct and we don't have a return	*/
  /* value address then we need to make one		        */

  if ((rvalue == NULL) && 
      (cif->rtype->type == FFI_TYPE_STRUCT))
    {
      /*@-sysunrecog@*/
      ecif.rvalue = alloca(cif->rtype->size);
      /*@=sysunrecog@*/
    }
  else
    ecif.rvalue = rvalue;
    
  
  switch (cif->abi) 
    {
    case FFI_SYSV:
    case FFI_GCC_SYSV:
      /*@-usedef@*/
      ffi_call_SYSV(&ecif, -cif->bytes, 
		    cif->flags, ecif.rvalue, fn);
      /*@=usedef@*/
      break;
    default:
      FFI_ASSERT(0);
      break;
    }
}
