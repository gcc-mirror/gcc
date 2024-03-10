#include <stdio.h>

#define N 100

int main (void)
{
  int a[N], b[N], c[N];

  for (int i = 0; i < N; i++) {
    a[i] = i * 2;
    b[i] = i * 3 + 1;
  }

  #pragma omp target enter data map (alloc: a, c)
    /* a has already been allocated, so this should be okay.  */
    #pragma omp target map (present, to: a)
      for (int i = 0; i < N; i++)
	c[i] = a[i];

    fprintf (stderr, "CheCKpOInT\n");
    /* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */

    /* b has not been allocated, so this should result in an error.  */
    /* { dg-output "libgomp: present clause: not present on the device \\\(0x\[0-9a-f\]+, \[0-9\]+\\\)" { target offload_device_nonshared_as } } */
    /* { dg-shouldfail "present error triggered" { offload_device_nonshared_as } } */
    #pragma omp target map (present, to: b)
      for (int i = 0; i < N; i++)
	c[i] += b[i];
  #pragma omp target exit data map (from: c)
}
