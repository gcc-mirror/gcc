// PR c++/36308
// { dg-do run }

#include <omp.h>
#include <assert.h>

#define N 10

struct B
{
  static int icount;
  static int ccount;
  static int dcount;
  static int xcount;

  B ();
  B (const B &);
  virtual ~B ();
  B& operator= (const B &);
  void doit ();
  static void clear () { icount = ccount = dcount = xcount = 0; }
};

int B::icount;
int B::ccount;
int B::dcount;
int B::xcount;

B::B ()
{
  #pragma omp atomic
    icount++;
}

B::B (const B &)
{
  #pragma omp atomic
    ccount++;
}

B::~B ()
{
  #pragma omp atomic
    dcount++;
}

void
B::doit ()
{
  #pragma omp atomic
    xcount++;
}

static int nthreads;

void
test1 ()
{
  B b[N];
  #pragma omp parallel private (b)
    {
      #pragma omp master
	nthreads = omp_get_num_threads ();
      b[0].doit ();
    }
}

void
test2 ()
{
  B b;
  #pragma omp parallel firstprivate (b)
    {
      #pragma omp single
	nthreads = omp_get_num_threads ();
      b.doit ();
    }
}

int
main ()
{
  omp_set_dynamic (0);
  omp_set_num_threads (4);

  B::clear ();
  test1 ();
  assert (B::xcount == nthreads);
  assert (B::ccount == 0);
  assert (B::icount == (nthreads + 1) * N);
  assert (B::dcount == (nthreads + 1) * N);

  B::clear ();
  test2 ();
  assert (B::xcount == nthreads);
  assert (B::ccount == nthreads);
  assert (B::icount == 1);
  assert (B::dcount == nthreads + 1);
  return 0;
}
