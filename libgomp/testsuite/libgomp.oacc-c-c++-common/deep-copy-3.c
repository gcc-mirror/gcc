#include <assert.h>
#include <stdlib.h>
#include <openacc.h>

int
main ()
{
  int n = 100, i;
  int *a = (int *) malloc (sizeof (int) * n);
  int *b;

  for (i = 0; i < n; i++)
    a[i] = i+1;

#pragma acc enter data copyin(a[:n]) create(b)

  b = a;
  acc_attach ((void **)&b);

#pragma acc parallel loop present (b[:n])
  for (i = 0; i < n; i++)
    b[i] = i+1;

  acc_detach ((void **)&b);

#pragma acc exit data copyout(a[:n], b)

  for (i = 0; i < 10; i++)
    assert (a[i] == b[i]);

  free (a);

  return 0;
}
