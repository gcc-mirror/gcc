/* { dg-do run } */

/* Make sure destructors are called for class variables bound
   in intervening code.  */

static int f1count[3], f2count[3];
static int g1count[3], g2count[3];

static int ccount[3], dcount[3];

class C {
 public:
  int n;
  C (int nn) { n = nn; ccount[n]++; }
  ~C () { dcount[n]++; n = 0; }
};

#ifndef __cplusplus
extern void abort (void);
#else
extern "C" void abort (void);
#endif

int f1 (int depth, int iter)
{
  f1count[depth]++;
  return iter;
}

int f2 (int depth, int iter)
{
  f2count[depth]++;
  return iter;
}

int g1 (int depth, int iter)
{
  g1count[depth]++;
  return iter;
}

int g2 (int depth, int iter)
{
  g2count[depth]++;
  return iter;
}

void s1 (int a1, int a2, int a3)
{
  int i, j, k;

#pragma omp for collapse(3)
  for (i = 0; i < a1; i++)
    {
      C local0(0);
      f1 (local0.n, i);
      {
	g1 (local0.n, i);
	for (j = 0; j < a2; j++)
	  {
	    C local1(1);
	    f1 (local1.n, j);
	    {
	      g1 (local1.n, j);
	      for (k = 0; k < a3; k++)
		{
		  C local2(2);
		  f1 (local2.n, k);
		  {
		    g1 (local2.n, k);
		    g2 (local2.n, k);
		  }
		  f2 (local2.n, k);
		}
	      g2 (local1.n, j);
	    }
	  f2 (local1.n, j);
	  }
	g2 (local0.n, i);
      }
      f2 (local0.n, i);
    }
}

int
main (void)
{
  f1count[0] = 0;
  f1count[1] = 0;
  f1count[2] = 0;
  f2count[0] = 0;
  f2count[1] = 0;
  f2count[2] = 0;

  g1count[0] = 0;
  g1count[1] = 0;
  g1count[2] = 0;
  g2count[0] = 0;
  g2count[1] = 0;
  g2count[2] = 0;

  s1 (3, 4, 5);

  /* All intervening code at the same depth must be executed the same
     number of times. */
  if (f1count[0] != f2count[0]) abort ();
  if (f1count[1] != f2count[1]) abort ();
  if (f1count[2] != f2count[2]) abort ();
  if (g1count[0] != f1count[0]) abort ();
  if (g2count[0] != f1count[0]) abort ();
  if (g1count[1] != f1count[1]) abort ();
  if (g2count[1] != f1count[1]) abort ();
  if (g1count[2] != f1count[2]) abort ();
  if (g2count[2] != f1count[2]) abort ();

  /* Intervening code must be executed at least as many times as the loop
     that encloses it. */
  if (f1count[0] < 3) abort ();
  if (f1count[1] < 3 * 4) abort ();

  /* Intervening code must not be executed more times than the number
     of logical iterations. */
  if (f1count[0] > 3 * 4 * 5) abort ();
  if (f1count[1] > 3 * 4 * 5) abort ();

  /* Check that the innermost loop body is executed exactly the number
     of logical iterations expected. */
  if (f1count[2] != 3 * 4 * 5) abort ();

  /* Check that each class object declared in intervening code was
     constructed and destructed an equal number of times.  */
  if (ccount[0] != dcount[0]) abort ();
  if (ccount[1] != dcount[1]) abort ();
  if (ccount[2] != dcount[2]) abort ();
}
