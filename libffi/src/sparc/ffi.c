/* -----------------------------------------------------------------------
   ffi.c - Copyright (c) 1996 Cygnus Solutions
   
   Sparc Foreign Function Interface 

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

void ffi_prep_args(char *stack, extended_cif *ecif)
{
  int i;
  int tmp;
  int avn;
  void **p_argv;
  char *argp;
  ffi_type **p_arg;

  tmp = 0;

  /* Skip 16 words for the window save area */
  argp = stack + 16*sizeof(void*);

  /* This should only really be done when we are returning a structure,
     however, it's faster just to do it all the time...

  if ( ecif->cif->rtype->type == FFI_TYPE_STRUCT ) */
  *(void **) argp = ecif->rvalue;

  /* And 1 word for the  structure return value. */
  argp += sizeof(void*);

#ifdef USING_PURIFY
  /* Purify will probably complain in our assembly routine, unless we
     zero out this memory. */

  ((int*)argp)[0] = 0;
  ((int*)argp)[1] = 0;
  ((int*)argp)[2] = 0;
  ((int*)argp)[3] = 0;
  ((int*)argp)[4] = 0;
  ((int*)argp)[5] = 0;
#endif

  avn = ecif->cif->nargs;
  p_argv = ecif->avalue;

  for (i = ecif->cif->nargs, p_arg = ecif->cif->arg_types;
       i && avn;
       i--, p_arg++)
    {
      size_t z;

      if (avn) 
	{
	  avn--;
	  if ((*p_arg)->type == FFI_TYPE_STRUCT
	      || (*p_arg)->type == FFI_TYPE_LONGDOUBLE)
	    {
	      *(unsigned int *) argp = (unsigned int)(* p_argv);
	      z = sizeof(void*);
	    }
	  else
	    {
	      z = (*p_arg)->size;
	      if (z < sizeof(int))
		{
		  z = sizeof(int);
		  switch ((*p_arg)->type)
		    {
		    case FFI_TYPE_SINT8:
		      *(signed int *) argp = *(SINT8 *)(* p_argv);
		      break;
		      
		    case FFI_TYPE_UINT8:
		      *(unsigned int *) argp = *(UINT8 *)(* p_argv);
		      break;
		      
		    case FFI_TYPE_SINT16:
		      *(signed int *) argp = *(SINT16 *)(* p_argv);
		      break;
		      
		    case FFI_TYPE_UINT16:
		      *(unsigned int *) argp = *(UINT16 *)(* p_argv);
		      break;
		      
		    case FFI_TYPE_SINT32:
		      *(signed int *) argp = *(SINT32 *)(* p_argv);
		      break;
		      
		    case FFI_TYPE_UINT32:
		      *(unsigned int *) argp = *(UINT32 *)(* p_argv);
		      break;
		      
		    default:
		      FFI_ASSERT(0);
		    }
		}
	      else
		{
		  memcpy(argp, *p_argv, z);
		}
	    }
	  p_argv++;
	  argp += z;
	}
    }
  
  return;
}

/* Perform machine dependent cif processing */
ffi_status ffi_prep_cif_machdep(ffi_cif *cif)
{
  /* If we are returning a struct, this will already have been added.
     Otherwise we need to add it because it's always got to be there! */

  if (cif->rtype->type != FFI_TYPE_STRUCT)
    cif->bytes += sizeof(void*);

  /* sparc call frames require that space is allocated for 6 args,
     even if they aren't used. Make that space if necessary. */
  
  if (cif->bytes < 4*6+4)
    cif->bytes = 4*6+4;

  /* Adjust cif->bytes. to include 16 words for the window save area,
     and maybe the struct/union return pointer area, */

  cif->bytes += 64;

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

extern int ffi_call_V8(void *, extended_cif *, unsigned, 
		       unsigned, unsigned *, void (*fn)());

void ffi_call(ffi_cif *cif, void (*fn)(), void *rvalue, void **avalue)
{
  extended_cif ecif;

  ecif.cif = cif;
  ecif.avalue = avalue;
  
  /* If the return value is a struct and we don't have a return	*/
  /* value address then we need to make one		        */
  
  if ((rvalue == NULL) && 
      (cif->rtype->type == FFI_TYPE_STRUCT))
    ecif.rvalue = alloca(cif->rtype->size);
  else
    ecif.rvalue = rvalue;
    
  switch (cif->abi) 
    {
    case FFI_V8:
      ffi_call_V8(ffi_prep_args, &ecif, cif->bytes, 
		  cif->flags, rvalue, fn);
      break;
    default:
      FFI_ASSERT(0);
      break;
    }
}
