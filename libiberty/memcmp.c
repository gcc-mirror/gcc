/* memcmp -- compare two memory regions.
   This function is in the public domain.  */

/*
NAME
	memcmp -- compare two memory regions

SYNOPSIS
	int memcmp (const void *from, const void *to, size_t count)

DESCRIPTION
	Compare two memory regions and return less than,
	equal to, or greater than zero, according to lexicographical
	ordering of the compared regions.
*/

#include <ansidecl.h>
#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#endif

int
DEFUN(memcmp, (str1, str2, count),
      const PTR str1 AND const PTR str2 AND size_t count)
{
  register unsigned char *s1 = (unsigned char*)str1;
  register unsigned char *s2 = (unsigned char*)str2;

  while (count-- > 0)
    {
      if (*s1++ != *s2++)
	  return s1[-1] < s2[-1] ? -1 : 1;
    }
  return 0;
}

