/*

@deftypefn Supplemental void* memchr (const void *@var{s}, int @var{c}, size_t @var{n})

This function searches memory starting at @code{*@var{s}} for the
character @var{c}.  The search only ends with the first occurrence of
@var{c}, or after @var{length} characters; in particular, a null
character does not terminate the search.  If the character @var{c} is
found within @var{length} characters of @code{*@var{s}}, a pointer
to the character is returned.  If @var{c} is not found, then @code{NULL} is
returned.

@end deftypefn

*/

#include <ansidecl.h>
#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#endif

PTR
memchr (src_void, c, length)
     register const PTR src_void;
     int c;
     size_t length;
{
  const unsigned char *src = (const unsigned char *)src_void;
  
  while (length-- > 0)
  {
    if (*src == c)
     return (PTR)src;
    src++;
  }
  return NULL;
}
