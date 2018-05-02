extern
#ifdef __cplusplus
"C"
#endif
void abort (void);
int v[16] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

int
main ()
{
  #pragma omp parallel num_threads (4)
  #pragma omp single
    {
      int i;
      #pragma omp taskgroup
      {
	for (i = 0; i < 16; i += 2)
	  #pragma omp task
	    {
	      #pragma omp task
		v[i]++;
	      #pragma omp task
		v[i + 1]++;
	    }
      }
      for (i = 0; i < 16; i++)
	if (v[i] != i + 2)
	  abort ();
      #pragma omp taskgroup
      {
	for (i = 0; i < 16; i += 2)
	  #pragma omp task
	    {
	      #pragma omp task
		v[i]++;
	      #pragma omp task
		v[i + 1]++;
	      #pragma omp taskwait
	    }
      }
      for (i = 0; i < 16; i++)
	if (v[i] != i + 3)
	  abort ();
      #pragma omp taskgroup
      {
	for (i = 0; i < 16; i += 2)
	  #pragma omp task
	    {
	      #pragma omp task
		v[i]++;
	      v[i + 1]++;
	    }
	#pragma omp taskwait
	for (i = 0; i < 16; i += 2)
	  #pragma omp task
	    v[i + 1]++;
      }
      for (i = 0; i < 16; i++)
	if (v[i] != i + 4 + (i & 1))
	  abort ();
      #pragma omp taskgroup
      {
	for (i = 0; i < 16; i += 2)
	  {
	    #pragma omp taskgroup
	      {
		#pragma omp task
		  v[i]++;
		#pragma omp task
		  v[i + 1]++;
	      }
	    if (v[i] != i + 5 || v[i + 1] != i + 7)
	      abort ();
	    #pragma omp task
	    v[i]++;
	  }
      }
      for (i = 0; i < 16; i++)
	if (v[i] != i + 6)
	  abort ();
    }
  return 0;
}
