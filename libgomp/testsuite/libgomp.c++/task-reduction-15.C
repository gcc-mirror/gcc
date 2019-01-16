extern "C" void abort ();

int as;
int &a = as;
long int bs = 1;
long int &b = bs;

template <typename T, typename U>
void
foo (T &c, U &d)
{
  T i;
  for (i = 0; i < 2; i++)
    #pragma omp task in_reduction (*: d) in_reduction (+: c) \
		     in_reduction (+: a) in_reduction (*: b)
    {
      a += 7;
      b *= 2;
      c += 9;
      d *= 3;
    }
}

template <typename T, typename U>
void
bar ()
{
  T cs = 0;
  T &c = cs;
  U ds = 1;
  #pragma omp parallel if (0)
  {
    U &d = ds;
    #pragma omp parallel
    {
      T i;
      #pragma omp for reduction (task, +: a, c) reduction (task, *: b, d)
      for (i = 0; i < 4; i++)
	#pragma omp task in_reduction (+: a, c) in_reduction (*: b, d)
	{
	  T j;
	  a += 7;
	  b *= 2;
	  for (j = 0; j < 2; j++)
	    #pragma omp task in_reduction (+: a, c) in_reduction (*: b, d)
	    {
	      a += 7;
	      b *= 2;
	      c += 9;
	      d *= 3;
	      foo (c, d);
	    }
	  c += 9;
	  d *= 3;
	}
#define THREEP4 (3LL * 3LL * 3LL * 3LL)
      if (d != (THREEP4 * THREEP4 * THREEP4 * THREEP4 * THREEP4 * THREEP4
		* THREEP4))
	abort ();
      if (a != 28 * 7 || b != (1L << 28) || c != 28 * 9)
	abort ();
    }
  }
  if (a != 28 * 7 || b != (1L << 28) || c != 28 * 9)
    abort ();
  if (ds != (THREEP4 * THREEP4 * THREEP4 * THREEP4 * THREEP4 * THREEP4
	     * THREEP4))
    abort ();
}

int
main ()
{
  bar<int, long long int> ();
}
