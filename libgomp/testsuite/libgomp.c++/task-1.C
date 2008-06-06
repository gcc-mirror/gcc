extern "C" void abort ();

int a = 18;

void
f1 (int i, int j, int k)
{
  int l = 6, m = 7, n = 8;
#pragma omp task private(j, m) shared(k, n)
  {
    j = 6;
    m = 5;
    if (++a != 19 || ++i != 9 || j != 6 || ++l != 7 || m != 5 || ++n != 9)
      #pragma omp atomic
	k++;
  }
#pragma omp taskwait
  if (a != 19 || i != 8 || j != 26 || k != 0 || l != 6 || m != 7 || n != 9)
    abort ();
}

int v1 = 1, v2 = 2, v5 = 5;
int err;

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
      #pragma omp task
	{
	  if (++v1 != 8 || ++v2 != 3 || ++v3 != 4 || ++v4 != 5 || ++v5 != 6)
	    err = 1;
	}
      #pragma omp taskwait
      if (v1 != 7 || v2 != 2 || v3 != 3 || v4 != 4 || v5 != 6)
	abort ();
      if (err)
	abort ();
    }
  }
}

void
f3 (int i, int j, int k)
{
  int l = 6, m = 7, n = 8;
#pragma omp task private(j, m) shared(k, n) untied
  {
    j = 6;
    m = 5;
    if (++a != 19 || ++i != 9 || j != 6 || ++l != 7 || m != 5 || ++n != 9)
      #pragma omp atomic
	k++;
  }
#pragma omp taskwait
  if (a != 19 || i != 8 || j != 26 || k != 0 || l != 6 || m != 7 || n != 9)
    abort ();
}

int
main ()
{
  f1 (8, 26, 0);
  f2 ();
  a = 18;
  f3 (8, 26, 0);
  a = 18;
#pragma omp parallel num_threads(4)
  {
    #pragma omp master
      {
	f1 (8, 26, 0);
	a = 18;
	f3 (8, 26, 0);
      }
  }
}
