/* { dg-do run } */

/* PR middle-end/63247 */

#include <stdlib.h>

int
main(int argc, char **argv)
{
#define N 4
    short a[N];

    a[0] = 10;
    a[1] = 10;
    a[2] = 10;
    a[3] = 10;

#pragma acc parallel copy(a[1:N-1])
    {
      a[1] = 51;
      a[2] = 52;
      a[3] = 53;
    }

    if (a[0] != 10)
      abort ();
    if (a[1] != 51)
      abort ();
    if (a[2] != 52)
      abort ();
    if (a[3] != 53)
      abort ();

    return 0;
}
