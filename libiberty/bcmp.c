/* bcmp
   This function is in the public domain.  */

/*

@deftypefn Supplemental int bcmp (char *@var{x}, char *@var{y}, int @var{count})

Compares the first @var{count} bytes of two areas of memory.  Returns
zero if they are the same, nonzero otherwise.  Returns zero if
@var{count} is zero.  A nonzero result only indicates a difference,
it does not indicate any sorting order (say, by having a positive
result mean @var{x} sorts before @var{y}).

@end deftypefn

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

