/* Portable version of bzero for systems without it.
   This function is in the public domain.  */

/*

@deftypefn Supplemental void bzero (char *@var{mem}, int @var{count})

Zeros @var{count} bytes starting at @var{mem}.  Use of this function
is deprecated in favor of @code{memset}.

@end deftypefn

*/


void
bzero (to, count)
  char *to;
  int count;
{
  while (count-- > 0)
    {
      *to++ = 0;
    }
}
