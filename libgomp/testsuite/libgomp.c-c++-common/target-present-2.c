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
    /* a and c have already been allocated, so this should be okay.  */
    #pragma omp target defaultmap (present)
      for (int i = 0; i < N; i++)
	c[i] = a[i];

    fprintf (stderr, "CheCKpOInT\n");
    /* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */

    /* b has not been allocated, so this should result in an error.  */
    /* { dg-output "libgomp: present clause: not present on the device \\(addr: 0x\[0-9a-f\]+, size: \[0-9\]+ \\(0x\[0-9a-f\]+\\), dev: \[0-9\]+\\\)" { target offload_device_nonshared_as } } */
    /* { dg-shouldfail "present error triggered" { offload_device_nonshared_as } } */
    #pragma omp target defaultmap (present)
      for (int i = 0; i < N; i++)
	c[i] += b[i];
  #pragma omp target exit data map (from: c)
}
