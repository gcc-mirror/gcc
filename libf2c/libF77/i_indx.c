#include "f2c.h"

integer
i_indx (char *a, char *b, ftnlen la, ftnlen lb)
{
  ftnlen i, n;
  char *s, *t, *bend;

  n = la - lb + 1;
  bend = b + lb;

  for (i = 0; i < n; ++i)
    {
      s = a + i;
      t = b;
      while (t < bend)
	if (*s++ != *t++)
	  goto no;
      return (i + 1);
    no:;
    }
  return (0);
}
