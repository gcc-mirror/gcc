/* Portable version of bzero for systems without it.
   This function is in the public domain.  */

/*
NAME
	bzero -- zero the contents of a specified memory region

SYNOPSIS
	void bzero (char *to, int count)

DESCRIPTION
	Zero COUNT bytes of memory pointed to by TO.

BUGS
	Significant speed enhancements may be made in some environments
	by zeroing more than a single byte at a time, or by unrolling the
	loop.

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
