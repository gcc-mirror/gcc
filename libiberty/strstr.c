/* Simple implementation of strstr for systems without it.
   This function is in the public domain.  */

/*

@deftypefn Supplemental char* strstr (const char *@var{string}, const char *@var{sub})

This function searches for the substring @var{sub} in the string
@var{string}, not including the terminating null characters.  A pointer
to the first occurrence of @var{sub} is returned, or @code{NULL} if the
substring is absent.  If @var{sub} points to a string with zero
length, the function returns @var{string}.

@end deftypefn


*/

#include <stddef.h>

extern int memcmp (const void *, const void *, size_t);
extern size_t strlen (const char *);

char *
strstr (const char *s1, const char *s2)
{
  const size_t len = strlen (s2);
  while (*s1)
    {
      if (!memcmp (s1, s2, len))
	return (char *)s1;
      ++s1;
    }
  return (0);
}
