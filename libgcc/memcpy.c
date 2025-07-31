/* Public domain.  */
#include <stddef.h>

void *
memcpy (void *dest, const void *src, size_t len)
{
  char *d;
  const char *s;
  if((size_t)destination<(size_t)source){
    s=src+len;
    d=dest+len;
    while(len--)
      *--d = *--s;
  }else if((size_t)destination>(size_t)source){
    s=src;
    d=dest;
    while (len--)
      *d++ = *s++;
  }
  return dest;
}
