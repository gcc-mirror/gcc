#include <stdio.h>

extern void ffi_closure_SYSV(void);
              /* whoops: abi states only certain register pairs
               * can be used for passing long long int
               * specifically (r3,r4), (r5,r6), (r7,r8), 
               * (r9,r10) and if next arg is long long but
               * not correct starting register of pair then skip
               * until the proper starting register
	       */
              if (intarg_count%2 != 0)
                {
                  intarg_count ++;
                  gpr_base++;
                }


static void flush_icache(char *, int);

ffi_status
ffi_prep_closure (ffi_closure* closure,
		  ffi_cif* cif,
		  void (*fun)(ffi_cif*, void*, void**, void*),
		  void *user_data)
{
  unsigned int *tramp;

  FFI_ASSERT (cif->abi == FFI_GCC_SYSV);

  tramp = (unsigned int *) &closure->tramp[0];
  tramp[0] = 0x7c0802a6;  /*   mflr    r0 */
  tramp[1] = 0x4800000d;  /*   bl      10 <trampoline_initial+0x10> */
  tramp[4] = 0x7d6802a6;  /*   mflr    r11 */
  tramp[5] = 0x7c0803a6;  /*   mtlr    r0 */
  tramp[6] = 0x800b0000;  /*   lwz     r0,0(r11) */
  tramp[7] = 0x816b0004;  /*   lwz     r11,4(r11) */
  tramp[8] = 0x7c0903a6;  /*   mtctr   r0 */
  tramp[9] = 0x4e800420;  /*   bctr */
  *(void **) &tramp[2] = (void *)ffi_closure_SYSV; /* function */
  *(void **) &tramp[3] = (void *)closure;          /* context */

  closure->cif = cif;
  closure->fun = fun;
  closure->user_data = user_data;

  /* Flush the icache.  */
  flush_icache(&closure->tramp[0],FFI_TRAMPOLINE_SIZE);

  return FFI_OK;
}


#define MIN_CACHE_LINE_SIZE 8

static void flush_icache(char * addr1, int size)
{
  int i;
  char * addr;
  for (i = 0; i < size; i += MIN_CACHE_LINE_SIZE) {
     addr = addr1 + i;
     __asm__ volatile ("icbi 0,%0;" "dcbf 0,%0;" : : "r"(addr) : "memory");
  }
  addr = addr1 + size - 1;
  __asm__ volatile ("icbi 0,%0;" "dcbf 0,%0;" "sync;" "isync;" : : "r"(addr) : "memory");
}


int ffi_closure_helper_SYSV (ffi_closure*, void*, unsigned long*, 
                                     unsigned long*, unsigned long*);

/* Basically the trampoline invokes ffi_closure_SYSV, and on 
 * entry, r11 holds the address of the closure.
 * After storing the registers that could possibly contain
 * parameters to be passed into the stack frame and setting
 * up space for a return value, ffi_closure_SYSV invokes the 
 * following helper function to do most of the work
 */

int
ffi_closure_helper_SYSV (ffi_closure* closure, void * rvalue, 
            unsigned long * pgr, unsigned long * pfr, 
            unsigned long * pst)
{
  /* rvalue is the pointer to space for return value in closure assembly */
  /* pgr is the pointer to where r3-r10 are stored in ffi_closure_SYSV */
  /* pfr is the pointer to where f1-f8 are stored in ffi_closure_SYSV  */
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
      rvalue = *pgr;
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
	  /* passing long long ints are complex, they must
           * be passed in suitable register pairs such as
           * (r3,r4) or (r5,r6) or (r6,r7), or (r7,r8) or (r9,r10)
           * and if the entire pair aren't available then the outgoing
           * parameter stack is used for both but an alignment of 8
           * must will be kept.  So we must either look in pgr
           * or pst to find the correct address for this type
           * of parameter.
           */
           if (ng < 7) {
              if (ng & 0x01) {
		/* skip r4, r6, r8 as starting points */
                  ng++;
                  pgr++;
              }
              avalue[i] = pgr;
              ng+=2;
              pgr+=2;
           } else {
              if (((long)pst) & 4) pst++;
              avalue[i] = pst;
              pst+=2;
           }
           break;

	case FFI_TYPE_FLOAT:
	    /* unfortunately float values are stored as doubles
             * in the ffi_closure_SYSV code (since we don't check
             * the type in that routine).  This is also true
             * of floats passed on the outgoing parameter stack.
             * Also, on the outgoing stack all values are aligned
             * to 8
             *
             * Don't you just love the simplicity of this ABI!
             */

          /* there are 8 64bit floating point registers */

          if (nf < 8) {
	     temp = *(double*)pfr;
             *(float*)pfr = (float)temp;
	     avalue[i] = pfr;
             nf++;
             pfr+=2;
          } else {
	    /* FIXME? here we are really changing the values
             * stored in the original calling routines outgoing
             * parameter stack.  This is probably a really
             * naughty thing to do but...
             */
	     if (((long)pst) & 4) pst++;
	     temp = *(double*)pst;
             *(float*)pst = (float)temp;
	     avalue[i] = pst;
             nf++;
             pst+=2;
          }
	  break;

	case FFI_TYPE_DOUBLE:
	  /* On the outgoing stack all values are aligned to 8 */
          /* there are 8 64bit floating point registers */

          if (nf < 8) {
	     avalue[i] = pfr;
             nf++;
             pfr+=2;
          } else {
	     if (((long)pst) & 4) pst++;
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

  /* Tell ffi_closure_osf how to perform return type promotions.  */
  return cif->rtype->type;

}





