/* { dg-additional-options "-O0 -fdump-tree-omplower" } */
/* { dg-require-alias "" } */
/* { dg-xfail-if "PR 97102/PR 97106 - .alias not (yet) supported for nvptx" { offload_target_nvptx } } */

#ifdef __cplusplus
extern "C" {
#endif

int foo () { return 42; }
int bar () __attribute__((alias ("foo")));
int baz () __attribute__((alias ("bar")));

#ifdef __cplusplus
}
#endif


int
main ()
{
  int n;
  #pragma omp target map(from:n)
    n = baz ();
  if (n != 42)
    __builtin_abort ();
}
/* { dg-final { scan-tree-dump-times "__attribute__..omp declare target" 1 "omplower" } } */
