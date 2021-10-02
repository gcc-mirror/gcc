/* Wrapper as usleep takes 'useconds_t', an unsigned integer type, as argument. */

#include <unistd.h>

void
my_usleep (int t)
{
  usleep (t);
}
