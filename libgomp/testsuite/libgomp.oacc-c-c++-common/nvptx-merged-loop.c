/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-options "-foffload=-fdump-rtl-mach" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */

#define N (32*32*32+17)
void __attribute__ ((noinline)) Foo (int *ary)
{
  int ix;

#pragma acc parallel num_workers(32) vector_length(32) copyout(ary[0:N])
  {
    /* Loop partitioning should be merged.  */
#pragma acc loop worker vector
    for (unsigned ix = 0; ix < N; ix++)
      {
	ary[ix] = ix;
      }
  }
}

int main ()
{
  int ary[N];

  Foo (ary);

  return 0;
}   

/* { dg-final { scan-offload-rtl-dump "Merging loop .* into " "mach" } } */
