/* { dg-do link } */
/* { dg-excess-errors "unresolved symbol foo, lto1, mkoffload and lto-wrapper fatal errors" { target { offload_target_nvptx || offload_target_amdgcn } } } */
/* { dg-additional-sources "function-not-offloaded-aux.c" } */

#pragma omp declare target
int var;
#pragma omp end declare target

extern void foo ();

int
main ()
{
#pragma omp target
  foo ();
}
