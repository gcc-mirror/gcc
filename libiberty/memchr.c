/*
FUNCTION
	<<memchr>>---find character in memory

INDEX
	memchr

ANSI_SYNOPSIS
	#include <string.h>
	void *memchr(const void *<[src]>, int <[c]>, size_t <[length]>);

TRAD_SYNOPSIS
	#include <string.h>
	void *memchr(<[src]>, <[c]>, <[length]>)
	void *<[src]>;
	void *<[c]>;
	size_t <[length]>;

DESCRIPTION
	This function searches memory starting at <<*<[src]>>> for the
	character <[c]>.  The search only ends with the first
	occurrence of <[c]>, or after <[length]> characters; in
	particular, <<NULL>> does not terminate the search.

RETURNS
	If the character <[c]> is found within <[length]> characters
	of <<*<[src]>>>, a pointer to the character is returned. If
	<[c]> is not found, then <<NULL>> is returned. 	

PORTABILITY
<<memchr>>  requires no supporting OS subroutines.

QUICKREF
	memchr ansi pure

*/

#include <ansidecl.h>
#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#endif

PTR
memchr (src_void, c, length)
     register CONST PTR src_void;
     int c;
     size_t length;
{
  CONST unsigned char *src = (CONST unsigned char *)src_void;
  
  while (--length >= 0)
  {
    if (*src == c)
     return (PTR)src;
    src++;
  }
  return NULL;
}
