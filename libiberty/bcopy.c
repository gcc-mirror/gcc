/* bcopy -- copy memory regions of arbitary length

@deftypefn Supplemental void bcopy (char *@var{in}, char *@var{out}, int @var{length})

Copies @var{length} bytes from memory region @var{in} to region
@var{out}.  The use of @code{bcopy} is deprecated in new programs.

@end deftypefn

*/

void
bcopy (src, dest, len)
  register char *src, *dest;
  int len;
{
  if (dest < src)
    while (len--)
      *dest++ = *src++;
  else
    {
      char *lasts = src + (len-1);
      char *lastd = dest + (len-1);
      while (len--)
        *(char *)lastd-- = *(char *)lasts--;
    }
}
