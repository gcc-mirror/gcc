extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

void
foo (int i, char *j)
{
  #pragma omp atomic
  j[i]++;
  #pragma omp ordered threads
  {
    int t;
    #pragma omp atomic read
    t = j[i];
    if (t != 3)
      abort ();
    if (i > 1)
      {
	#pragma omp atomic read
	t = j[i - 1];
	if (t == 2)
	  abort ();
      }
    if (i < 127)
      {
	#pragma omp atomic read
	t = j[i + 1];
	if (t == 4)
	  abort ();
      }
  }
  #pragma omp atomic
  j[i]++;
}

int
main ()
{
  int i;
  char j[128];
  #pragma omp parallel
  {
    #pragma omp for
    for (i = 0; i < 128; i++)
      j[i] = 0;
    #pragma omp for ordered schedule(dynamic, 1)
    for (i = 0; i < 128; i++)
      {
	#pragma omp atomic
	j[i]++;
	#pragma omp ordered threads
	{
	  int t;
	  #pragma omp atomic read
	  t = j[i];
	  if (t != 1)
	    abort ();
	  if (i > 1)
	    {
	      #pragma omp atomic read
	      t = j[i - 1];
	      if (t == 0)
		abort ();
	    }
	  if (i < 127)
	    {
	      #pragma omp atomic read
	      t = j[i + 1];
	      if (t == 2)
		abort ();
	    }
	}
	#pragma omp atomic
	j[i]++;
      }
    #pragma omp for ordered schedule(static, 1)
    for (i = 0; i < 128; i++)
      foo (i, j);
  }
  return 0;
}
