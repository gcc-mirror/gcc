// { dg-do run }

extern "C" void abort (void);

template <typename I, typename F>
void
foo ()
{
  I j = -10000;
  F f = 1024.0;
  int i;
  #pragma omp parallel for reduction (min:f) reduction (max:j)
    for (i = 0; i < 4; i++)
      switch (i)
	{
	case 0:
	  if (j < -16) j = -16; break;
	case 1:
	  if (f > -2.0) f = -2.0; break;
	case 2:
	  if (j < 8) j = 8; if (f > 9.0) f = 9.0; break;
	case 3:
	  break;
	}
  if (j != 8 || f != -2.0)
    abort ();
}

int
main ()
{
  int j = -10000;
  float f = 1024.0;
  int i;
  #pragma omp parallel for reduction (min:f) reduction (max:j)
    for (i = 0; i < 4; i++)
      switch (i)
	{
	case 0:
	  if (j < -16) j = -16; break;
	case 1:
	  if (f > -2.0) f = -2.0; break;
	case 2:
	  if (j < 8) j = 8; if (f > 9.0) f = 9.0; break;
	case 3:
	  break;
	}
  if (j != 8 || f != -2.0)
    abort ();
  foo <int, float> ();
  foo <long, double> ();
  foo <long long, long double> ();
  return 0;
}
