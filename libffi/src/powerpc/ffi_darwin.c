/* -----------------------------------------------------------------------
   ffi.c - Copyright (c) 1998 Geoffrey Keating
   
   PowerPC Foreign Function Interface 

   Darwin ABI support (c) 2001 John Hornkvist
   AIX ABI support (c) 2002 Free Software Foundation, Inc.

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
   
extern void ffi_closure_ASM(void);

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

/* About the DARWIN ABI.  */
enum {
  NUM_GPR_ARG_REGISTERS = 8,
  NUM_FPR_ARG_REGISTERS = 13
};
enum { ASM_NEEDS_REGISTERS = 4 };

/* ffi_prep_args is called by the assembly routine once stack space
   has been allocated for the function's arguments.

   The stack layout we want looks like this:

   |   Return address from ffi_call_DARWIN      |	higher addresses
   |--------------------------------------------|
   |   Previous backchain pointer	4	| 	stack pointer here
   |--------------------------------------------|<+ <<<	on entry to
   |   Saved r28-r31			4*4	| |	ffi_call_DARWIN
   |--------------------------------------------| |
   |   Parameters             (at least 8*4=32) | |
   |--------------------------------------------| |
   |   Space for GPR2                   4       | |
   |--------------------------------------------| |	stack	|
   |   Reserved                       2*4       | |	grows	|
   |--------------------------------------------| |	down	V
   |   Space for callee's LR		4	| |
   |--------------------------------------------| |	lower addresses	
   |   Saved CR                         4       | |
   |--------------------------------------------| |     stack pointer here
   |   Current backchain pointer	4	|-/	during
   |--------------------------------------------|   <<<	ffi_call_DARWIN

   */

/*@-exportheader@*/
void ffi_prep_args(extended_cif *ecif, unsigned *const stack)
/*@=exportheader@*/
{
  const unsigned bytes = ecif->cif->bytes;
  const unsigned flags = ecif->cif->flags; 

  /* 'stacktop' points at the previous backchain pointer.  */
  unsigned *const stacktop = stack + (ecif->cif->bytes / sizeof(unsigned));

  /* 'fpr_base' points at the space for fpr1, and grows upwards as
     we use FPR registers.  */
  double *fpr_base = (double*) (stacktop - ASM_NEEDS_REGISTERS) - NUM_FPR_ARG_REGISTERS;
  int fparg_count = 0;


  /* 'next_arg' grows up as we put parameters in it.  */
  unsigned *next_arg = stack + 6; // 6 reserved posistions. 

  int i=ecif->cif->nargs;
  double double_tmp;
  float float_tmp;
  void **p_argv = ecif->avalue;
  unsigned gprvalue;
  ffi_type** ptr = ecif->cif->arg_types;

  /* Check that everything starts aligned properly.  */
  FFI_ASSERT(((unsigned)(char *)stack & 0xF) == 0);
  FFI_ASSERT(((unsigned)(char *)stacktop & 0xF) == 0);
  FFI_ASSERT((bytes & 0xF) == 0);

  /* Deal with return values that are actually pass-by-reference.  */
  // Rule:
  // Return values are referenced by r3, so r4 is the first parameter.
  if (flags & FLAG_RETVAL_REFERENCE)
    *next_arg++ = (unsigned)(char *)ecif->rvalue;

  /* Now for the arguments.  */
  for (;
       i > 0;
       i--, ptr++, p_argv++)
    {
      switch ((*ptr)->type)
	{
	/* If a floating-point parameter appears before all of the general-
	   purpose registers are filled, the corresponding GPRs that match
	   the size of the floating-point parameter are skipped.  */
	case FFI_TYPE_FLOAT:
	  double_tmp = *(float *)*p_argv;
	  if (fparg_count >= NUM_FPR_ARG_REGISTERS)
            *(double *)next_arg = double_tmp;
	  else
            *fpr_base++ = double_tmp;
          next_arg++;
	  fparg_count++;
	  FFI_ASSERT(flags & FLAG_FP_ARGUMENTS);
	  break;
	case FFI_TYPE_DOUBLE:
	  double_tmp = *(double *)*p_argv;
	  if (fparg_count >= NUM_FPR_ARG_REGISTERS)
            *(double *)next_arg = double_tmp;
	  else
            *fpr_base++ = double_tmp;
          next_arg += 2;
	  fparg_count++;
	  FFI_ASSERT(flags & FLAG_FP_ARGUMENTS);
	  break;

	case FFI_TYPE_UINT64:
	case FFI_TYPE_SINT64:
          *(long long *)next_arg = *(long long *)*p_argv;
          next_arg+=2;
	  break;
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

        case FFI_TYPE_STRUCT:

#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
        case FFI_TYPE_LONGDOUBLE:
#endif
	  
	  memcpy((char*)next_arg, (char *)*p_argv, (*ptr)->size);
	  next_arg+=(((((*ptr)->size) + 3) & ~0x3)/4);	
        break;	  
	  
	case FFI_TYPE_INT:
   	case FFI_TYPE_UINT32:
	case FFI_TYPE_SINT32:
	case FFI_TYPE_POINTER:
	  gprvalue = *(unsigned *)*p_argv;
	putgpr:
	  *next_arg++ = gprvalue;
	  break;
	default:
	  break;
	}
    }

  /* Check that we didn't overrun the stack...  */
  //FFI_ASSERT(copy_space >= (char *)next_arg);
  //FFI_ASSERT(gpr_base <= stacktop - ASM_NEEDS_REGISTERS);
  //FFI_ASSERT((unsigned *)fpr_base
  //	     <= stacktop - ASM_NEEDS_REGISTERS - NUM_GPR_ARG_REGISTERS);
  //FFI_ASSERT(flags & FLAG_4_GPR_ARGUMENTS || intarg_count <= 4);
}

/* Perform machine dependent cif processing */
ffi_status ffi_prep_cif_machdep(ffi_cif *cif)
{
  /* All this is for the DARWIN ABI.  */
  int i;
  ffi_type **ptr;
  unsigned bytes;
  int fparg_count = 0, intarg_count = 0;
  unsigned flags = 0;
  unsigned struct_copy_size = 0;

  /* All the machine-independent calculation of cif->bytes will be wrong.
     Redo the calculation for DARWIN.  */

  /* Space for the frame pointer, callee's LR, CR, etc, and for 
     the asm's temp regs.  */

  bytes = (6 + ASM_NEEDS_REGISTERS) * sizeof(long);

  /* Return value handling.  The rules are as follows:
     - 32-bit (or less) integer values are returned in gpr3;
     - Structures of size <= 4 bytes also returned in gpr3;
     - 64-bit integer values and structures between 5 and 8 bytes are returned
       in gpr3 and gpr4;
     - Single/double FP values are returned in fpr1;
     - Long double FP (if not equivalent to double) values are returned in
       fpr1 and fpr2;
     - Larger structures values are allocated space and a pointer is passed
       as the first argument.  */
  switch (cif->rtype->type)
    {
#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
    case FFI_TYPE_LONGDOUBLE:
#endif
      /* Fall through.  */
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
      flags |= FLAG_RETVAL_REFERENCE;
      flags |= FLAG_RETURNS_NOTHING;
      intarg_count++;
      break;
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
	  intarg_count+=(((*ptr)->size + 3) & ~0x3)/4;
	  break;

	default:
	  /* Everything else is passed as a 4-byte word in a GPR, either
	     the object itself or a pointer to it.  */
	  intarg_count++;
	  break;
	}
    }

  if (fparg_count != 0)
    flags |= FLAG_FP_ARGUMENTS;
  if (struct_copy_size != 0)
    flags |= FLAG_ARG_NEEDS_COPY;
  
  /* Space for the FPR registers, if needed.  */
  if (fparg_count != 0)
    bytes += NUM_FPR_ARG_REGISTERS * sizeof(double);

  /* Stack space.  */
  if ((intarg_count + 2 * fparg_count) > NUM_GPR_ARG_REGISTERS)
    bytes += (intarg_count + 2 * fparg_count) * sizeof(long);
  else
    bytes += NUM_GPR_ARG_REGISTERS * sizeof(long);

  /* The stack space allocated needs to be a multiple of 16 bytes.  */
  bytes = (bytes + 15) & ~0xF;

  cif->flags = flags;
  cif->bytes = bytes;
  
  return FFI_OK;
}

/*@-declundef@*/
/*@-exportheader@*/
extern void ffi_call_AIX(/*@out@*/ extended_cif *, 
			 unsigned, unsigned, 
			 /*@out@*/ unsigned *, 
			 void (*fn)(),
			 void (*fn2)());
extern void ffi_call_DARWIN(/*@out@*/ extended_cif *, 
			    unsigned, unsigned, 
			    /*@out@*/ unsigned *, 
			    void (*fn)(),
			    void (*fn2)());
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
    case FFI_AIX:
      /*@-usedef@*/
      ffi_call_AIX(&ecif, -cif->bytes, 
		   cif->flags, ecif.rvalue, fn, ffi_prep_args);
      /*@=usedef@*/
      break;
    case FFI_DARWIN:
      /*@-usedef@*/
      ffi_call_DARWIN(&ecif, -cif->bytes, 
		      cif->flags, ecif.rvalue, fn, ffi_prep_args);
      /*@=usedef@*/
      break;
    default:
      FFI_ASSERT(0);
      break;
    }
}

static void flush_icache(char *);
static void flush_range(char *, int);
   
/* The layout of a function descriptor.  A C function pointer really    */
/* points to one of these.                                              */

typedef struct aix_fd_struct {
    void *code_pointer;
    void *toc;
} aix_fd;

/* here I'd like to add the stack frame layout we use in darwin_closure.S
 * and aix_clsoure.S
 *
/* SP previous -> +---------------------------------------+ <--- child frame
                  | back chain to caller 4                | 
                  +---------------------------------------+ 4
                  | saved CR 4                            | 
                  +---------------------------------------+ 8 
                  | saved LR 4                            | 
                  +---------------------------------------+ 12
                  | reserved for compilers 4              | 
                  +---------------------------------------+ 16
                  | reserved for binders 4                | 
                  +---------------------------------------+ 20
                  | saved TOC pointer 4                   | 
                  +---------------------------------------+ 24
                  | always reserved 8*4=32 (previous GPRs)| 
                  | according to the linkage convention   |
                  | from AIX                              |
                  +---------------------------------------+ 56
                  | our FPR area 13*8=104                 |
                  | f1                                    |
                  | .                                     |
                  | f13                                   | 
                  +---------------------------------------+ 160
                  | result area 8                         |
                  +---------------------------------------+ 168
                  | alignement to the next multiple of 16 |
SP current -->    +---------------------------------------+ 176 <- parent frame
                  | back chain to caller 4                | 
                  +---------------------------------------+ 180
                  | saved CR 4                            | 
                  +---------------------------------------+ 184
                  | saved LR 4                            | 
                  +---------------------------------------+ 188
                  | reserved for compilers 4              | 
                  +---------------------------------------+ 192
                  | reserved for binders 4                | 
                  +---------------------------------------+ 196
                  | saved TOC pointer 4                   | 
                  +---------------------------------------+ 200
                  | always reserved 8*4=32  we store our  |
                  | GPRs here                             |
                  | r3                                    |
                  | .                                     |
                  | r10                                   |
                  +---------------------------------------+ 232
                  | PST area, overflow part               | 
                  +---------------------------------------+ xxx
                  | ????                                  | 
                  +---------------------------------------+ xxx

*/
ffi_status
ffi_prep_closure (ffi_closure* closure,    
                  ffi_cif* cif,
                  void (*fun)(ffi_cif*, void*, void**, void*),
                  void *user_data)
{
  unsigned int *tramp;
  struct ffi_aix_trampoline_struct *tramp_aix;
  aix_fd *fd;
 
  switch (cif->abi)
    {  
    case FFI_DARWIN:

      FFI_ASSERT (cif->abi == FFI_DARWIN);

      tramp = (unsigned int *) &closure->tramp[0];
      tramp[0] = 0x7c0802a6;  /*   mflr    r0 */
      tramp[1] = 0x4800000d;  /*   bl      10 <trampoline_initial+0x10> */
      tramp[4] = 0x7d6802a6;  /*   mflr    r11 */
      tramp[5] = 0x818b0000;  /*   lwz     r12,0(r11)  /* function address */
      tramp[6] = 0x7c0803a6;  /*   mtlr    r0  */
      tramp[7] = 0x7d8903a6;  /*   mtctr   r12 */
      tramp[8] = 0x816b0004;  /*   lwz     r11,4(r11)  /* static chain */
      tramp[9] = 0x4e800420;  /*   bctr */
      *(void **) &tramp[2] = (void *)ffi_closure_ASM; /* function */
      *(void **) &tramp[3] = (void *)closure;          /* context */

      closure->cif = cif;
      closure->fun = fun;
      closure->user_data = user_data;

      /* Flush the icache. Only necessary on Darwin  */
      flush_range(&closure->tramp[0],FFI_TRAMPOLINE_SIZE);
     
      break;

    case FFI_AIX:

      tramp_aix = (struct ffi_aix_trampoline_struct *) (closure->tramp);
      fd = (aix_fd *)(void *)ffi_closure_ASM;

      FFI_ASSERT (cif->abi == FFI_AIX);

      tramp_aix->code_pointer = fd->code_pointer;
      tramp_aix->toc = fd->toc;
      tramp_aix->static_chain = closure;
      closure->cif = cif;
      closure->fun = fun;
      closure->user_data = user_data;

    default:

      FFI_ASSERT(0);
      break;
    }
  return FFI_OK;
}

static void
flush_icache(char *addr)
{
#ifndef _AIX
  __asm__ volatile (
                "dcbf 0,%0;"
                "sync;"
                "icbi 0,%0;"
                "sync;"
                "isync;"
                : : "r"(addr) : "memory");
#endif
}

static void
flush_range(char * addr1, int size)
{
#define MIN_LINE_SIZE 32
  int i;
  for (i = 0; i < size; i += MIN_LINE_SIZE)
    flush_icache(addr1+i);
  flush_icache(addr1+size-1);
}

int ffi_closure_helper_DARWIN (ffi_closure*, void*, unsigned long*,
                                     unsigned long*, unsigned long*);

/* Basically the trampoline invokes ffi_closure_ASM, and on
 * entry, r11 holds the address of the closure.
 * After storing the registers that could possibly contain
 * parameters to be passed into the stack frame and setting
 * up space for a return value, ffi_closure_ASM invokes the
 * following helper function to do most of the work
 */

int
ffi_closure_helper_DARWIN (ffi_closure* closure, void * rvalue,
            unsigned long * pgr, unsigned long * pfr,
            unsigned long * pst)
{
  /* rvalue is the pointer to space for return value in closure assembly */
  /* pgr is the pointer to where r3-r10 are stored in ffi_closure_ASM */
  /* pfr is the pointer to where f1-f13 are stored in ffi_closure_ASM */
  /* pst is the pointer to outgoing parameter stack in original caller */

  void **          avalue;
  ffi_type **      arg_types;
  long             i, avn;
  long             nf;   /* number of floating registers already used */
  long             ng;   /* number of general registers already used */
  ffi_cif *        cif;
  double           temp;

  cif = closure->cif;
  avalue = alloca(cif->nargs * sizeof(void *));

  nf = 0;
  ng = 0;

  /* Copy the caller's structure return value address so that the closure
     returns the data directly to the caller.  */
  if (cif->rtype->type == FFI_TYPE_STRUCT)
    {
      rvalue = (void *)pgr;
      ng++;      
      pgr++;
    }

  i = 0;
  avn = cif->nargs;
  arg_types = cif->arg_types;

  /* Grab the addresses of the arguments from the stack frame.  */
  while (i < avn)
    {
      switch (arg_types[i]->type)
        {
        case FFI_TYPE_SINT8:
        case FFI_TYPE_UINT8:
        /* there are 8 gpr registers used to pass values */
          if (ng < 8) {
             avalue[i] = (((char *)pgr)+3);
             ng++;
             pgr++;
          } else {
             avalue[i] = (((char *)pst)+3);
             pst++;
          }
          break;

        case FFI_TYPE_SINT16:
        case FFI_TYPE_UINT16:
        /* there are 8 gpr registers used to pass values */
          if (ng < 8) {
             avalue[i] = (((char *)pgr)+2);
             ng++;
             pgr++;
          } else {
             avalue[i] = (((char *)pst)+2);
             pst++;
          }
          break;

        case FFI_TYPE_SINT32:
        case FFI_TYPE_UINT32:
        case FFI_TYPE_POINTER:
        case FFI_TYPE_STRUCT:
        /* there are 8 gpr registers used to pass values */
          if (ng < 8) {  
             avalue[i] = pgr;
             ng++;
             pgr++;
          } else {
             avalue[i] = pst;
             pst++;
          }
          break;

        case FFI_TYPE_SINT64:
        case FFI_TYPE_UINT64:
          /* long long ints are passed in two gpr's if available or in 
           * the pst, one place is a bit odd, when a long long passes
	   * the boundary between gpr and pst area we have to increment
	   * the pst by one.
           */
           if (ng < 7) {
              avalue[i] = pgr;
              ng+=2;
              pgr+=2;
           } else if (ng == 7) {
              avalue[i] = pgr;
              ng++;
              pgr++;
              pst++;
           } else {
              avalue[i] = pst;
              pst+=2;
           }
           break;

        case FFI_TYPE_FLOAT:
          /* a float value consumes a GPR
           *
           * there are 13 64bit floating point registers 
	   */
          
	  if ((ng > 7) && (nf < 13)) {
	     pst++;
	  }
          if (nf < 13) {
	     temp = *(double*)pfr;
             *(float*)pfr = (float)temp;
             avalue[i] = pfr;
             nf++;
             pfr+=2;
             ng++;
	     pgr++;
	         
          } else {
             avalue[i] = pst;
             nf++;
             pst++;
          }
          break;

        case FFI_TYPE_DOUBLE:
	  /* a double value consumes two GPRs
           *  
           * there are 13 64bit floating point registers 
	   */

	  if ((ng == 7) && (nf < 13)) {
             pst++;	/* if only one gpr is left the double steals it */
	  } else if ((ng > 7) && (nf < 13)) {
	     pst+=2;	/* a double consumes two GPRs in Darwin/AIX */
	  }
          if (nf < 13) {
             avalue[i] = pfr;
             nf++;
             pfr+=2;
	     ng+=2;
	     pgr+=2;

          } else {
             avalue[i] = pst;
             nf++;
             pst+=2;
          }
          break;

        default:
          FFI_ASSERT(0);
       
        }

      i++;
    }

  (closure->fun) (cif, rvalue, avalue, closure->user_data);

  /* Tell ffi_closure_ASM to perform return type promotions.  */
  return cif->rtype->type;

}
