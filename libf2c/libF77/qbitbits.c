#include "f2c.h"

#ifndef LONGBITS
#define LONGBITS 32
#endif

#ifndef LONG8BITS
#define LONG8BITS (2*LONGBITS)
#endif

integer
qbit_bits (longint a, integer b, integer len)
{
  /* Assume 2's complement arithmetic */

  ulongint x, y;

  x = (ulongint) a;
  y = (ulongint) - 1L;
  x >>= b;
  y <<= len;
  return (longint) (x & y);
}

longint
qbit_cshift (longint a, integer b, integer len)
{
  ulongint x, y, z;

  x = (ulongint) a;
  if (len <= 0)
    {
      if (len == 0)
	return 0;
      goto full_len;
    }
  if (len >= LONG8BITS)
    {
    full_len:
      if (b >= 0)
	{
	  b %= LONG8BITS;
	  return (longint) (x << b | x >> (LONG8BITS - b));
	}
      b = -b;
      b %= LONG8BITS;
      return (longint) (x << (LONG8BITS - b) | x >> b);
    }
  y = z = (unsigned long) -1;
  y <<= len;
  z &= ~y;
  y &= x;
  x &= z;
  if (b >= 0)
    {
      b %= len;
      return (longint) (y | (z & (x << b | x >> (len - b))));
    }
  b = -b;
  b %= len;
  return (longint) (y | (z & (x >> b | x << (len - b))));
}
