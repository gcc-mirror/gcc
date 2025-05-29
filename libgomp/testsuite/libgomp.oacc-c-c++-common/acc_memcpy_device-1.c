/* { dg-prune-output "using .vector_length \\(32\\)" } */

/* PR libgomp/93226  */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <openacc.h>

enum { N = 1024 };

static int D[N];
#pragma acc declare device_resident(D)

#pragma acc routine
intptr_t init_d()
{
  for (int i = 0; i < N; i++)
    D[i] = 27*i;
  return (intptr_t) &D[0];
}

int
main ()
{
  int *a, *b, *e;
  void *d_a, *d_b, *d_c, *d_d, *d_e, *d_f;
  intptr_t intptr;
  bool fail = false;

  a = (int *) malloc (N*sizeof (int));
  b = (int *) malloc (N*sizeof (int));
  e = (int *) malloc (N*sizeof (int));
  d_c = acc_malloc (N*sizeof (int));
  d_f = acc_malloc (N*sizeof (int));

  memset (e, 0xff, N*sizeof (int));
  d_e = acc_copyin (e, N*sizeof (int));

  #pragma acc serial copyout(intptr)
    intptr = init_d ();
  d_d = (void*) intptr;
  acc_memcpy_device (d_c, d_d, N*sizeof (int));

  #pragma acc serial copy(fail) deviceptr(d_c) firstprivate(intptr)
  {
    int *cc = (int *) d_c;
    int *dd = (int *) intptr;
    for (int i = 0; i < N; i++)
      if (dd[i] != 27*i || cc[i] != 27*i)
	{
	  fail = true;
	  __builtin_abort ();
	}
  }
  if (fail) __builtin_abort ();

  for (int i = 0; i < N; i++)
    a[i] = 11*i;
  for (int i = 0; i < N; i++)
    b[i] = 31*i;

  d_a = acc_copyin (a, N*sizeof (int));
  acc_copyin_async (b, N*sizeof (int), acc_async_noval);

  #pragma acc parallel deviceptr(d_c) async
  {
    int *cc = (int *) d_c;
    #pragma acc loop
    for (int i = 0; i < N; i++)
      cc[i] = -17*i;
  }

  acc_memcpy_device_async (d_d, d_a, N*sizeof (int), acc_async_noval);
  acc_memcpy_device_async (d_f, d_c, N*sizeof (int), acc_async_noval);
  acc_wait (acc_async_noval);
  d_b = acc_deviceptr (b);
  acc_memcpy_device_async (d_e, d_b, N*sizeof (int), acc_async_noval);
  acc_wait (acc_async_noval);

  #pragma acc serial deviceptr(d_d, d_e, d_f) copy(fail)
  {
    int *dd = (int *) d_d;
    int *ee = (int *) d_e;
    int *ff = (int *) d_f;
    for (int i = 0; i < N; i++)
      if (dd[i] != 11*i
	  || ee[i] != 31*i
	  || ff[i] != -17*i)
	{
	  fail = true;
	  __builtin_abort ();
	}
  }
  if (fail) __builtin_abort ();
}
