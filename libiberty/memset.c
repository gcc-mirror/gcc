/* memset
   This implementation is in the public domain.  */

/*

@deftypefn Supplemental void* memset (void *@var{s}, int @var{c}, size_t @var{count})

Sets the first @var{count} bytes of @var{s} to the constant byte
@var{c}, returning a pointer to @var{s}.

@end deftypefn

*/

#include <ansidecl.h>
#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#endif

PTR
DEFUN(memset, (dest, val, len),
      PTR dest AND register int val AND register size_t len)
{
  register unsigned char *ptr = (unsigned char*)dest;
  while (len-- > 0)
    *ptr++ = val;
  return dest;
}
