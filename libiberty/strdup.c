/*

@deftypefn Supplemental char* strdup (const char *@var{s})

Returns a pointer to a copy of @var{s} in memory obtained from
@code{malloc}, or @code{NULL} if insufficient memory was available.

@end deftypefn

*/

#include <ansidecl.h>
#ifdef ANSI_PROTOTYPES
#include <stddef.h>
#else
#define size_t unsigned long
#endif

extern size_t	strlen PARAMS ((const char*));
extern PTR	malloc PARAMS ((size_t));
extern PTR	memcpy PARAMS ((PTR, const PTR, size_t));

char *
strdup(s)
     const char *s;
{
  size_t len = strlen (s) + 1;
  char *result = (char*) malloc (len);
  if (result == (char*) 0)
    return (char*) 0;
  return (char*) memcpy (result, s, len);
}
