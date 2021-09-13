/* { dg-do run { target openacc_nvidia_accel_selected } }
   { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-additional-options "-foffload=-fdump-rtl-mach" } */

/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

int a;
#pragma acc declare create(a)

#pragma acc routine vector
/* { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .+2 } */
void __attribute__((noinline, noclone))
foo_v (void)
{
  a = 1;
}

#pragma acc routine worker
/* { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .+3 }
   { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .+2 } */
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
