extern void abort (void);

int a = 18;

void
f1 (int i, int j, int k)
{
  int l = 6, m = 7, n = 8;
#pragma omp parallel private(j, m) shared(k, n) firstprivate(i, l) \
	    num_threads(1)
  {
    j = 6;
    m = 5;
    if (++a != 19 || ++i != 9 || j != 6 || ++l != 7 || m != 5 || ++n != 9)
      #pragma omp atomic
	k++;
  }
  if (a != 19 || i != 8 || j != 26 || k != 0 || l != 6 || m != 7 || n != 9)
    abort ();
}

int v1 = 1, v2 = 2, v5 = 5;
int e;

void
f2 (void)
{
  int v3 = 3;
#pragma omp sections private (v1) firstprivate (v2)
  {
  #pragma omp section
    {
      int v4 = 4;
      v1 = 7;
      #pragma omp parallel num_threads(1) firstprivate(v1, v2, v3, v4)
	{
	  if (++v1 != 8 || ++v2 != 3 || ++v3 != 4 || ++v4 != 5 || ++v5 != 6)
	    e = 1;
	}
      if (v1 != 7 || v2 != 2 || v3 != 3 || v4 != 4 || v5 != 6)
	abort ();
      if (e)
	abort ();
    }
  }
}

int
main (void)
{
  f1 (8, 26, 0);
  f2 ();
  return 0;
}
