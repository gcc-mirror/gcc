/* { dg-do run }  */
/* { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } } */

/* Test that the reverse offload message buffers can cope with multiple
   requests from multiple kernels.  */

#pragma omp requires reverse_offload

int main ()
{
  for (int n=0; n < 5; n++)
    {
      #pragma omp target teams distribute parallel for nowait collapse(2)
      for (int i=0; i < 32; i++)
	for (int j=0; j < 16; j++)
	  {
	    int val = 0;
	    #pragma omp target device ( ancestor:1 ) firstprivate(i,j) map(from:val)
	    {
	      val = i + j;
	    }

	    if (val != i + j)
	      __builtin_abort ();
	  }
    }

#pragma omp taskwait

  return 0;
}
