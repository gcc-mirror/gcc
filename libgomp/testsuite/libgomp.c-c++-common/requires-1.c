/* { dg-additional-sources requires-1-aux.c } */
/* { dg-require-effective-target omp_usm } */

#pragma omp requires unified_shared_memory

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

/* { dg-output "libgomp: requires-directive clause inconsistency between compilation units detected" } */
/* { dg-prune-output "device does not support required features" } */
