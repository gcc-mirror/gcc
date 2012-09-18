/* Portable version of strnlen.
   This function is in the public domain.  */

/*

@deftypefn Supplemental size_t strnlen (const char *@var{s}, size_t @var{maxlen})

Returns the length of @var{s}, as with @code{strlen}, but never looks
past the first @var{maxlen} characters in the string.  If there is no
'\0' character in the first @var{maxlen} characters, returns
@var{maxlen}.

@end deftypefn

*/

#include "config.h"

#include <stddef.h>

size_t
strnlen (const char *s, size_t maxlen)
{
  size_t i;

  for (i = 0; i < maxlen; ++i)
    if (s[i] == '\0')
      break;
  return i;
}
