/* { dg-do run }  */
/* { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } } */

/* Test that the reverse offload message buffers can cope with a lot of
   requests.  */

#pragma omp requires reverse_offload

int main ()
{
  #pragma omp target teams distribute parallel for collapse(2)
  for (int i=0; i < 100; i++)
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

  return 0;
}
