/* xmemdup.c -- Duplicate a memory buffer, using xcalloc.
   This trivial function is in the public domain.
   Jeff Garzik, September 1999.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "ansidecl.h"
#include "libiberty.h"

#include <sys/types.h> /* For size_t. */
#ifdef HAVE_STRING_H
#include <string.h>
#endif

PTR
xmemdup (input, copy_size, alloc_size)
  const PTR input;
  size_t copy_size;
  size_t alloc_size;
{
  PTR output = xcalloc (1, alloc_size);
  memcpy (output, input, copy_size);
  return output;
}
