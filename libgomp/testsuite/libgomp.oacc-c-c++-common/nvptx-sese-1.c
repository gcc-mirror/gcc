/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-options "-foffload=-fdump-rtl-mach" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */

#pragma acc routine  seq
int __attribute__((noinline)) foo (int x)
{
  return x & 2;
}

int main ()
{
  int r = 0;
  
#pragma acc parallel copy(r) vector_length(32)
  {
#pragma acc loop vector reduction (+:r)
    for (int i = 00; i < 40; i++)
      r += i;

    /* This piece is a multi-block SESE region */
    if (foo (r))
      r *= 2;

    if (r & 1) /* to here. */
#pragma acc loop vector reduction (+:r)
      for (int i = 00; i < 40; i++)
	r += i;
  }

  return 0;
}

/* Match {N->N(.N)+} */
/* { dg-final { scan-offload-rtl-dump "SESE regions:.* \[0-9\]+{\[0-9\]+->\[0-9\]+(\\.\[0-9\]+)+}" "mach" } } */
