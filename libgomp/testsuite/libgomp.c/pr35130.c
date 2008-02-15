/* PR middle-end/35130 */

extern void abort (void);

void
f1 (void)
{
  int a[4], k;
  void nested (int x)
  {
    a[x] = 42;
  }

  for (k = 0; k < 4; k++)
    a[k] = 0;
#pragma omp parallel for
  for (k = 0; k < 4; k++)
    nested (k);

  if (a[0] != 42 || a[1] != 42 || a[2] != 42 || a[3] != 42)
    abort ();
}

void
f2 (void)
{
  int a[4], k;
  void nested (void)
  {
    int l;
    void nested2 (int x)
    {
      a[x] = 42;
    }
#pragma omp parallel for
    for (l = 0; l < 4; l++)
      nested2 (l);
  }

  for (k = 0; k < 4; k++)
    a[k] = 0;

  nested ();

  if (a[0] != 42 || a[1] != 42 || a[2] != 42 || a[3] != 42)
    abort ();
}

void
f3 (void)
{
  int a[4], b[4], c[4], k;
  void nested (int x)
  {
    a[x] = b[x] = c[x] = 42;
  }

  for (k = 0; k < 4; k++)
    a[k] = b[k] = c[k] = 0;
  nested (0);

#pragma omp parallel
  {
  #pragma omp single
    {
      a[1] = 43;
      b[1] = 43;
    }
  #pragma omp parallel
    {
    #pragma omp single
      {
	b[2] = 44;
	c[2] = 44;
      }
    }
  }

  if (a[0] != 42 || a[1] != 43 || a[2] != 0 || a[3] != 0)
    abort ();
  if (b[0] != 42 || b[1] != 43 || b[2] != 44 || b[3] != 0)
    abort ();
  if (c[0] != 42 || c[1] != 0 || c[2] != 44 || c[3] != 0)
    abort ();
}

void
f4 (void)
{
  int a[4], b[4], c[4], k;
  void nested ()
  {
  #pragma omp parallel
    {
    #pragma omp single
      {
	a[1] = 43;
	b[1] = 43;
      }
    #pragma omp parallel
      {
      #pragma omp single
	{
	  b[2] = 44;
	  c[2] = 44;
	}
      }
    }
  }

  for (k = 0; k < 4; k++)
    a[k] = b[k] = c[k] = k == 0 ? 42 : 0;
  nested ();

  if (a[0] != 42 || a[1] != 43 || a[2] != 0 || a[3] != 0)
    abort ();
  if (b[0] != 42 || b[1] != 43 || b[2] != 44 || b[3] != 0)
    abort ();
  if (c[0] != 42 || c[1] != 0 || c[2] != 44 || c[3] != 0)
    abort ();
}

int
main (void)
{
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  return 0;
}
