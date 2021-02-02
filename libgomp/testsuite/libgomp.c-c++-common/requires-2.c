/* { dg-additional-sources requires-2-aux.c } */

#pragma omp requires reverse_offload

int a[10];
extern void foo (void);

int
main (void)
{
  #pragma omp target
  for (int i = 0; i < 10; i++)
    a[i] = 0;

  foo ();
  return 0;
}

/* { dg-output "libgomp: device does not support required features: reverse_offload" } */
