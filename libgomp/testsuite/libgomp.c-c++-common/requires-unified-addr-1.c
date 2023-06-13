/* PR libgomp/109837 */

#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#pragma omp requires unified_address

#define N 15

void
test_device (int dev)
{
  struct st {
    int *ptr;
    int n;
  };
  struct st s;

  s.n = 10;
  s.ptr = (int *) omp_target_alloc (sizeof (int)*s.n, dev);
  int *ptr1 = (int *) omp_target_alloc (sizeof (int)*N, dev);
  assert (s.ptr != NULL);
  assert (ptr1 != NULL);

  int q[4] = {1,2,3,4};
  int *qptr;
  #pragma omp target enter data map(q) device(device_num: dev)
  #pragma omp target data use_device_addr(q) device(device_num: dev)
    qptr = q;

  #pragma omp target map(to:s) device(device_num: dev)
  for (int i = 0; i < s.n; i++)
    s.ptr[i] = 23*i;

  int *ptr2 = &s.ptr[3];

  #pragma omp target firstprivate(qptr) map(tofrom:ptr2) device(device_num: dev)
  for (int i = 0; i < 4; i++)
    *(qptr++) = ptr2[i];

  #pragma omp target exit data map(q) device(device_num: dev)
  for (int i = 0; i < 4; i++)
    q[i] = 23 * (i+3);

  #pragma omp target map(to: ptr1) device(device_num: dev)
  for (int i = 0; i < N; i++)
    ptr1[i] = 11*i;

  int *ptr3 = (int *) malloc (sizeof (int)*N);
  assert (0 == omp_target_memcpy(ptr3, ptr1, N * sizeof(int), 0, 0,
				 omp_get_initial_device(), dev));
  for (int i = 0; i < N; i++)
    assert (ptr3[i] == 11*i);

  free (ptr3);
  omp_target_free (ptr1, dev);
  omp_target_free (s.ptr, dev);
}

int
main()
{
  int ntgts = omp_get_num_devices();
  if (ntgts)
    fprintf (stderr, "Offloading devices exist\n");  /* { dg-output "Offloading devices exist(\n|\r\n|\r)" { target offload_device } } */
  else
    fprintf (stderr, "Only host fallback\n");        /* { dg-output "Only host fallback(\n|\r\n|\r)" { target { ! offload_device } } } */

  for (int i = 0; i <= ntgts; i++)
    test_device (i);
  return 0;
}
