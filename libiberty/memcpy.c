/* memcpy (the standard C function)
   This function is in the public domain.  */

/*
NAME
	memcpy -- copy memory regions of arbitary length

SYNOPSIS
	void* memcpy (void *out, const void *in, size_t n);

DESCRIPTION
	Copy LENGTH bytes from memory region pointed to by IN to memory
	region pointed to by OUT.
*/

#include <ansidecl.h>
#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#endif

PTR
DEFUN(memcpy, (out, in, length), PTR out AND const PTR in AND size_t length)
{
    bcopy(in, out, length);
    return out;
}
