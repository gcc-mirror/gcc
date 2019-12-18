/* Verify that 'acc_delete' etc. with a 'NULL' address is a no-op.  */

#include <assert.h>
#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;

  unsigned char *a = (unsigned char *) malloc (N);
  assert (a);

  void *a_d = acc_create (a, N);
  assert (a_d);

  acc_delete (NULL, N);
  assert (acc_is_present (a, N));
  //TODO similar for others.

  acc_delete (a, N);
  free (a);

  return 0;
}
