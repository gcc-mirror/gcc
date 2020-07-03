/* { dg-do run { target openacc_nvidia_accel_selected } }
   { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-additional-options "-foffload=-fdump-rtl-mach" } */

int a;
#pragma acc declare create(a)

#pragma acc routine vector
void __attribute__((noinline, noclone))
foo_v (void)
{
  a = 1;
}

#pragma acc routine worker
void __attribute__((noinline, noclone))
foo_w (void)
{
  a = 2;
}

int
main (void)
{

  #pragma acc parallel
  foo_v ();

  #pragma acc parallel
  foo_w ();

  return 0;
}

/* { dg-final { scan-offload-rtl-dump-not "nvptx_barsync" "mach" } } */
