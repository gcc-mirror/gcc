#include "f2c.h"

#ifndef LONGBITS
#define LONGBITS 32
#endif

integer
lbit_bits (integer a, integer b, integer len)
{
  /* Assume 2's complement arithmetic */

  unsigned long x, y;

  x = (unsigned long) a;
  y = (unsigned long) -1L;
  x >>= b;
  y <<= len;
  return (integer) (x & ~y);
}

integer
lbit_cshift (integer a, integer b, integer len)
{
  unsigned long x, y, z;

  x = (unsigned long) a;
  if (len <= 0)
    {
      if (len == 0)
	return 0;
      goto full_len;
    }
  if (len >= LONGBITS)
    {
    full_len:
      if (b >= 0)
	{
	  b %= LONGBITS;
	  return (integer) (x << b | x >> (LONGBITS - b));
	}
      b = -b;
      b %= LONGBITS;
      return (integer) (x << (LONGBITS - b) | x >> b);
    }
  y = z = (unsigned long) -1;
  y <<= len;
  z &= ~y;
  y &= x;
  x &= z;
  if (b >= 0)
    {
      b %= len;
      return (integer) (y | (z & (x << b | x >> (len - b))));
    }
  b = -b;
  b %= len;
  return (integer) (y | (z & (x >> b | x << (len - b))));
}
