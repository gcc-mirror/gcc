/* -----------------------------------------------------------------------
   ffi.c - Copyright (c) 1998 Cygnus Solutions
   
   Alpha Foreign Function Interface 

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
   IN NO EVENT SHALL CYGNUS SOLUTIONS BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------- */

#include <ffi.h>
#include <ffi_common.h>

#include <stdlib.h>

/* ffi_prep_args is called by the assembly routine once stack space
   has been allocated for the function's arguments */

static void
ffi_prep_args(char *stack, extended_cif *ecif, int bytes, int flags)
{
  register long i, avn;
  register void **p_argv;
  register char *argp;
  register ffi_type **p_arg;

  /* To streamline things in the assembly code, we always allocate 12
     words for loading up the int and fp argument registers.  The layout
     is as when processing varargs: the 6 fp args, the 6 int args, then
     the incoming stack.  ARGP points to the first int slot.  */
  argp = stack + 6 * SIZEOF_ARG;
  memset (stack, 0, 12 * SIZEOF_ARG);

  if ( ecif->cif->rtype->type == FFI_TYPE_STRUCT )
    {
      *(void **) argp = ecif->rvalue;
      argp += sizeof(void *);
    }

  i = 0;
  avn = ecif->cif->nargs;
  p_arg = ecif->cif->arg_types;
  p_argv = ecif->avalue;
  while (i < avn)
    {
      size_t z = ALIGN((*p_arg)->size, SIZEOF_ARG);

      switch ((*p_arg)->type)
	{
	case FFI_TYPE_SINT8:
	  *(SINT64 *) argp = *(SINT8 *)(* p_argv);
	  break;
		  
	case FFI_TYPE_UINT8:
	  *(UINT64 *) argp = *(UINT8 *)(* p_argv);
	  break;
		  
	case FFI_TYPE_SINT16:
	  *(SINT64 *) argp = *(SINT16 *)(* p_argv);
	  break;
		  
	case FFI_TYPE_UINT16:
	  *(UINT64 *) argp = *(UINT16 *)(* p_argv);
	  break;
		  
	case FFI_TYPE_SINT32:
	  *(SINT64 *) argp = *(SINT32 *)(* p_argv);
	  break;
		  
	case FFI_TYPE_UINT32:
	  *(UINT64 *) argp = *(UINT32 *)(* p_argv);
	  break;

	case FFI_TYPE_SINT64:
	case FFI_TYPE_UINT64:
	case FFI_TYPE_POINTER:
	  *(UINT64 *) argp = *(UINT64 *)(* p_argv);
	  break;

	case FFI_TYPE_FLOAT:
	  if (argp - stack < 12 * SIZEOF_ARG)
	    {
	      /* Note the conversion -- all the fp regs are loaded as
		 doubles.  The in-register format is the same.  */
	      *(double *) (argp - 6 * SIZEOF_ARG) = *(float *)(* p_argv);
	    }
	  else
	    *(float *) argp = *(float *)(* p_argv);
	  break;

	case FFI_TYPE_DOUBLE:
	  if (argp - stack < 12 * SIZEOF_ARG)
	    *(double *) (argp - 6 * SIZEOF_ARG) = *(double *)(* p_argv);
	  else
	    *(double *) argp = *(double *)(* p_argv);
	  break;

	case FFI_TYPE_STRUCT:
	  memcpy(argp, *p_argv, (*p_arg)->size);
	  break;

	default:
	  FFI_ASSERT(0);
	}

      argp += z;
      i++, p_arg++, p_argv++;
    }
}

/* Perform machine dependent cif processing */
ffi_status
ffi_prep_cif_machdep(ffi_cif *cif)
{
  /* Adjust cif->bytes. to include 12 words for the temporary register
     argument loading area.  This will be removed before the call.  */

  cif->bytes += 6*SIZEOF_ARG;
  if (cif->bytes < 12*SIZEOF_ARG)
    cif->bytes = 12*SIZEOF_ARG;

  /* The stack must be double word aligned, so round bytes up
     appropriately. */

  cif->bytes = ALIGN(cif->bytes, 2*sizeof(void*));

  /* Set the return type flag */
  switch (cif->rtype->type)
    {
    case FFI_TYPE_VOID:
    case FFI_TYPE_STRUCT:
      cif->flags = cif->rtype->type;
      break;

    case FFI_TYPE_FLOAT:
      cif->flags = FFI_TYPE_FLOAT;
      break;

    case FFI_TYPE_DOUBLE:
      cif->flags = FFI_TYPE_DOUBLE;
      break;

    default:
      cif->flags = FFI_TYPE_INT;
      break;
    }
  
  return FFI_OK;
}

extern int ffi_call_osf(void (*)(char *, extended_cif *, int, int), 
			extended_cif *, unsigned, 
			unsigned, unsigned *, void (*)());

void
ffi_call(ffi_cif *cif, void (*fn)(), void *rvalue, void **avalue)
{
  extended_cif ecif;

  ecif.cif = cif;
  ecif.avalue = avalue;
  
  /* If the return value is a struct and we don't have a return
     value address then we need to make one.  */
  
  if (rvalue == NULL && cif->rtype->type == FFI_TYPE_STRUCT)
    ecif.rvalue = alloca(cif->rtype->size);
  else
    ecif.rvalue = rvalue;
    
  switch (cif->abi) 
    {
    case FFI_OSF:
      ffi_call_osf(ffi_prep_args, &ecif, cif->bytes, 
		   cif->flags, rvalue, fn);
      break;

    default:
      FFI_ASSERT(0);
      break;
    }
}
