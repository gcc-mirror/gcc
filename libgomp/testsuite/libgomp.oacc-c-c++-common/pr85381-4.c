/* { dg-do run { target openacc_nvidia_accel_selected } }
   { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-additional-options "-foffload=-fdump-rtl-mach" } */

#define n 1024

int
main (void)
{
  #pragma acc parallel
  {
    #pragma acc loop worker
    for (int i = 0; i < n; i++)
      ;

    #pragma acc loop worker
    for (int i = 0; i < n; i++)
      ;
  }

  return 0;
}

/* Atm, %ntid.y is broadcast from one loop to the next, so there are 2 bar.syncs
   for that (the other two are there for the same reason as in pr85381-2.c).
   Todo: Recompute %ntid.y instead of broadcasting it. */
/* { dg-final { scan-offload-rtl-dump-times "nvptx_barsync" 4 "mach" } } */
