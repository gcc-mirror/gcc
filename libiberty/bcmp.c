/* bcmp
   This function is in the public domain.  */

/*

NAME

	bcmp -- compare two memory regions

SYNOPSIS

	int bcmp (char *from, char *to, int count)

DESCRIPTION

	Compare two memory regions and return zero if they are identical,
	non-zero otherwise.  If count is zero, return zero.

NOTES

	No guarantee is made about the non-zero returned value.  In
	particular, the results may be signficantly different than
	strcmp(), where the return value is guaranteed to be less than,
	equal to, or greater than zero, according to lexicographical
	sorting of the compared regions.

BUGS

*/


int
bcmp (from, to, count)
  char *from, *to;
  int count;
{
  int rtnval = 0;

  while (count-- > 0)
    {
      if (*from++ != *to++)
	{
	  rtnval = 1;
	  break;
	}
    }
  return (rtnval);
}

