/* { dg-additional-options "-save-temps -w" } */
/* { dg-do run { target openacc_nvidia_accel_selected } }
   { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */

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

/* { dg-final { scan-assembler-times "bar.sync" 0 } } */
