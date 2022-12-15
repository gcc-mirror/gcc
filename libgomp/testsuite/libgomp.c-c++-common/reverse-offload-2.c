/* { dg-do run }  */
/* { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } } */

#pragma omp requires reverse_offload

int
main ()
{
  int A[10];
  int y;

  for (int i = 0; i < 10; i++)
    A[i] = 2*i;

  y = 42;

  /* Pointlessly copy to the default device.  */
  #pragma omp target data map(to: A)
  {
    /* Not enclosed in a target region (= i.e. running on the host); the
       following is valid - it runs on the current device (= host).  */
    #pragma omp target device ( ancestor:1 ) firstprivate(y) map(to: A)
    {
      if (y != 42)
	__builtin_abort ();
      for (int i = 0; i < 10; i++)
	if (A[i] != 2*i)
	  __builtin_abort ();
      for (int i = 0; i < 10; i++)
	if (A[i] != 2*i)
	  A[i] = 4*i;
      y = 31;
    }

    if (y != 42)
      __builtin_abort ();
    for (int i = 0; i < 10; i++)
      if (A[i] != 2*i)
	__builtin_abort ();
  }

  if (y != 42)
    __builtin_abort ();
  for (int i = 0; i < 10; i++)
    if (A[i] != 2*i)
      __builtin_abort ();

  return 0;
}
