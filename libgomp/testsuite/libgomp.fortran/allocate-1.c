#include <stdint.h>

int
is_64bit_aligned (uintptr_t a)
{
  return ( (a & 0x3f) == 0);
}
