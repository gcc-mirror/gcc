/* EFL support routine to compare two character strings */

#include "f2c.h"

extern integer s_cmp (char *, char *, ftnlen, ftnlen);
integer
G77_ef1cmc_0 (ftnint * a, ftnlen * la, ftnint * b, ftnlen * lb)
{
  return (s_cmp ((char *) a, (char *) b, *la, *lb));
}
