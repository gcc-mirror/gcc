/* strncmp -- compare two strings, stop after n bytes.
   This function is in the public domain.  */

/*

@deftypefn Supplemental int strncmp (const char *@var{s1}, const char *@var{s2}, size_t @var{n})

Compares the first @var{n} bytes of two strings, returning a value as
@code{strcmp}.

@end deftypefn

*/

#include <ansidecl.h>
#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#endif

int
strncmp(s1, s2, n)
     const char *s1, *s2;
     register size_t n;
{
  register unsigned char u1, u2;

  while (n-- > 0)
    {
      u1 = (unsigned char) *s1++;
      u2 = (unsigned char) *s2++;
      if (u1 != u2)
	return u1 - u2;
      if (u1 == '\0')
	return 0;
    }
  return 0;
}
