// { dg-do run }

extern "C" void abort ();

struct A
{
  A ();
  ~A ();
  A (const A &);
  unsigned long l;
};

int e;

A::A ()
{
  l = 17;
}

A::~A ()
{
  if (l > 30)
    #pragma omp atomic
      e++;
}

A::A (const A &r)
{
  l = r.l;
}

void
check (int i, A &a, int j, A &b)
{
  if (i != 6 || a.l != 21 || j != 0 || b.l != 23)
    #pragma omp atomic
      e++;
}

A b;
int j;

void
foo (int i)
{
  A a;
  a.l = 21;
  #pragma omp task firstprivate (i, a, j, b)
    check (i, a, j, b);
}

void
bar (int i, A a)
{
  a.l = 21;
  #pragma omp task firstprivate (i, a, j, b)
    check (i, a, j, b);
}

A
baz ()
{
  A a, c;
  a.l = 21;
  c.l = 23;
  #pragma omp task firstprivate (a, c)
    check (6, a, 0, c);
  return a;
}

int
main ()
{
  b.l = 23;
  foo (6);
  bar (6, A ());
  baz ();
  #pragma omp parallel num_threads (4)
    {
      #pragma omp single
	for (int i = 0; i < 64; i++)
	  {
	    foo (6);
	    bar (6, A ());
	    baz ();
	  }
    }
  if (e)
    abort ();
}
