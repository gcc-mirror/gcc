/*

@deftypefn Supplemental void* memrchr (const void *@var{s}, int @var{c}, @
  size_t @var{n})

This function searches memory for the character @var{c} in reverse order,
starting at @code{*@var{s}+@var{n}-1} .  The search only ends with
the first occurrence of @var{c}, or when the start us reached; in particular,
a null character does not terminate the search.  If the character @var{c} is
found within @var{length} characters of @code{*@var{s}}, a pointer
to the character is returned.  If @var{c} is not found, then @code{NULL} is
returned.

@end deftypefn

*/

#include <ansidecl.h>
#include <stddef.h>

void *
memrchr (const void *src_void, int c, size_t length)
{
  if (length == 0)
    return NULL;

  const unsigned char *p = (const unsigned char*)src_void;
  p += length;
  while (*--p != (unsigned char)c)
    if (src_void == p)
      return NULL;
  return (void *)p;
}
