// { dg-do run }

#include <omp.h>
#include <assert.h>

struct B
{
  static int ic, dc, xc, ac, cc;

  B();
  B(const B &);
  ~B();
  B& operator=(const B &);
  void doit();
  static void clear();
};

int B::ic;
int B::dc;
int B::xc;
int B::cc;
int B::ac;

B::B()
{
  #pragma omp atomic
    ic++;
}

B::~B()
{
  #pragma omp atomic
    dc++;
}

B::B(const B &)
{
  #pragma omp atomic
    cc++;
}

B& B::operator=(const B &)
{
  #pragma omp atomic
    ac++;
  return *this;
}

void B::doit()
{
  #pragma omp atomic
    xc++;
}

void B::clear()
{
  ic = 0;
  dc = 0;
  cc = 0;
  ac = 0;
  xc = 0;
}

static int n;

void f1(B &a)
{
  B b;
  B &c = b;
  #pragma omp parallel default(none) private(a, c) shared (n)
    {
      #pragma omp master
	n = omp_get_num_threads ();
      a.doit();
      c.doit();
    }
}

void f2(B &a)
{
  B b;
  B &c = b;
  #pragma omp parallel default(none) firstprivate(a, c) shared(n)
    {
      #pragma omp master
	n = omp_get_num_threads ();
      a.doit();
      c.doit();
    }
}

void f3(B &a)
{
  B b;
  B &c = b;
  #pragma omp parallel default(none) shared(n, a, c)
    {
      #pragma omp master
	n = omp_get_num_threads ();
      #pragma omp for lastprivate (a, c)
      for (int i = 0; i < omp_get_num_threads (); i++)
	{
	  a.doit();
	  c.doit();
	}
    }
}

void f4()
{
  B b;
  B &c = b;
  #pragma omp parallel default(none) private (c) shared (n)
    {
      B d;
      B &e = d;
      #pragma omp single copyprivate (c, e)
      {
	c.doit();
	e.doit();
      }
      c.doit();
      e.doit();
    }
}

void f5(B (&a)[2])
{
  B b[2];
  B (&c)[2] = b;
  #pragma omp parallel default(none) private(a, c) shared (n)
    {
      #pragma omp master
	n = omp_get_num_threads ();
      a[0].doit();
      a[1].doit();
      c[0].doit();
      c[1].doit();
    }
}

void f6(B (&a)[2])
{
  B b[2];
  B (&c)[2] = b;
  #pragma omp parallel default(none) firstprivate(a, c) shared (n)
    {
      #pragma omp master
	n = omp_get_num_threads ();
      a[0].doit();
      a[1].doit();
      c[0].doit();
      c[1].doit();
    }
}

void f7(B (&a)[2])
{
  B b[2];
  B (&c)[2] = b;
  #pragma omp parallel default(none) shared(n, a, c)
    {
      #pragma omp master
	n = omp_get_num_threads ();
      #pragma omp for lastprivate (a, c)
      for (int i = 0; i < omp_get_num_threads (); i++)
	{
	  a[0].doit();
	  a[1].doit();
	  c[0].doit();
	  c[1].doit();
	}
    }
}

void f8()
{
  B b[2];
  B (&c)[2] = b;
  #pragma omp parallel default(none) private (c) shared (n)
    {
      B d[2];
      B (&e)[2] = d;
      #pragma omp single copyprivate (c, e)
      {
	c[0].doit();
	c[1].doit();
	e[0].doit();
	e[1].doit();
      }
      c[0].doit();
      c[1].doit();
      e[0].doit();
      e[1].doit();
    }
}

int main()
{
  {
    B a;
    f1(a);
  }
  assert (B::xc == 2*n && B::ic == 2*n+2 && B::dc == 2*n+2 && B::ac == 0 && B::cc == 0);
  B::clear();
  {
    B a;
    f2(a);
  }
  assert (B::xc == 2*n && B::ic == 2 && B::dc == 2*n+2 && B::ac == 0 && B::cc == 2*n);
  B::clear();
  {
    B a;
    f3(a);
  }
  assert (B::xc == 2*n && B::ic == 2*n+2 && B::dc == 2*n+2 && B::ac == 2 && B::cc == 0);
  B::clear();
  f4();
  assert (B::xc == 2*n+2 && B::ic == 2*n+1 && B::dc == 2*n+1 && B::ac == 2*n-2 && B::cc == 0);
  B::clear();
  {
    B a[2];
    f5(a);
  }
  assert (B::xc == 4*n && B::ic == 4*n+4 && B::dc == 4*n+4 && B::ac == 0 && B::cc == 0);
  B::clear();
  {
    B a[2];
    f6(a);
  }
  assert (B::xc == 4*n && B::ic == 4 && B::dc == 4*n+4 && B::ac == 0 && B::cc == 4*n);
  B::clear();
  {
    B a[2];
    f7(a);
  }
  assert (B::xc == 4*n && B::ic == 4*n+4 && B::dc == 4*n+4 && B::ac == 4 && B::cc == 0);
  B::clear();
  f8();
  assert (B::xc == 4*n+4 && B::ic == 4*n+2 && B::dc == 4*n+2 && B::ac == 4*n-4 && B::cc == 0);
  return 0;
}
