#include "f2c.h"

extern integer s_cmp (char *, char *, ftnlen, ftnlen);
logical
l_lt (char *a, char *b, ftnlen la, ftnlen lb)
{
  return (s_cmp (a, b, la, lb) < 0);
}
