#include "ansidecl.h"
#include "libiberty.h"
 
#ifdef ANSI_PROTOTYPES
#include <stddef.h>
#else
#define size_t unsigned long
#endif

/* For systems with larger pointers than ints, this must be declared.  */
PTR malloc PARAMS ((size_t));

PTR
calloc (nelem, elsize)
  size_t nelem, elsize;
{
  register PTR ptr;  

  if (nelem == 0 || elsize == 0)
    nelem = elsize = 1;
  
  ptr = malloc (nelem * elsize);
  if (ptr) bzero (ptr, nelem * elsize);
  
  return ptr;
}
