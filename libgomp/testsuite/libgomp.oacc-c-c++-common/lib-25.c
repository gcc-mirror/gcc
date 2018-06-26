/* Exercise acc_create and acc_delete.  */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  unsigned char *h;
  void *d;

  h = (unsigned char *) malloc (N);

  d = acc_create (h, N);
  if (!d)
    abort ();

  d = acc_create (h, N);
  if (!d)
    abort ();

  acc_delete (h, N);

  if (!acc_is_present (h, N))
    abort ();

  acc_delete (h, N);

#if !ACC_MEM_SHARED
  if (acc_is_present (h, N))
    abort ();
#endif

  free (h);

  return 0;
}
