/* { dg-do run { target offload_target_any } } */
/* { dg-shouldfail "present error triggered" } */

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

  /* This should fail as b has not been allocated.  */
  /* { dg-output "libgomp: present clause: !omp_target_is_present \\\(0x\[0-9a-f\]+, \[0-9\]+\\\)" } */
  #pragma omp target update to (present: b)

  #pragma omp target exit data map (from: c)
}
