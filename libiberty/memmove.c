/* Wrapper to implement ANSI C's memmove using BSD's bcopy. */
/* This function is in the public domain.  --Per Bothner. */
#include <ansidecl.h>
#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#endif

PTR
memmove (s1, s2, n)
     PTR s1;
     const PTR s2;
     size_t n;
{
  bcopy (s2, s1, n);
  return s1;
}
