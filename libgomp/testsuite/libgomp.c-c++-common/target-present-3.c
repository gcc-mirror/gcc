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

  /* This should work as a has already been allocated.  */
  #pragma omp target update to (present: a)

  #pragma omp target map(present,alloc: a, c)
  for (int i = 0; i < N; i++) {
    if (a[i] != i * 2)
      __builtin_abort ();
    c[i] = 23*i;
  }

  #pragma omp target update from(present : c)
  for (int i = 0; i < N; i++) {
    if (c[i] != 23*i)
      __builtin_abort ();
  }

  fprintf (stderr, "CheCKpOInT\n");
  /* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */

  /* This should fail as b has not been allocated.  */
    /* { dg-output "libgomp: present clause: not present on the device \\(addr: 0x\[0-9a-f\]+, size: \[0-9\]+ \\(0x\[0-9a-f\]+\\), dev: \[0-9\]+\\\)" { target offload_device_nonshared_as } } */
  /* { dg-shouldfail "present error triggered" { offload_device_nonshared_as } } */
  #pragma omp target update to (present: b)

  #pragma omp target exit data map (from: c)
}
