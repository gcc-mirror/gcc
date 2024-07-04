// { dg-do run }
// { dg-options "-fopenmp" }

#include <cassert>

struct S
{
  int x[10];
};

struct T
{
  struct S ***s;
};

struct U
{
  struct T **t;
};

void
foo (void)
{
  U *u = new U;
  T *real_t = new T;
  S *real_s = new S;
  T **t_pp = &real_t;
  S **s_pp = &real_s;
  S ***s_ppp = &s_pp;
  u->t = t_pp;
  (*u->t)->s = s_ppp;
  for (int i = 0; i < 10; i++)
    (**((*u->t)->s))->x[i] = 0;
#pragma omp target map(u->t, *u->t, (*u->t)->s, *(*u->t)->s, **(*u->t)->s, \
		       (**(*u->t)->s)->x[0:10])
  for (int i = 0; i < 10; i++)
    (**((*u->t)->s))->x[i] = i * 3;
  for (int i = 0; i < 10; i++)
    assert ((**((*u->t)->s))->x[i] == i * 3);
  delete real_s;
  delete real_t;
  delete u;
}

template<typename X>
struct St
{
  X x[10];
};

template<typename X>
struct Tt
{
  X ***s;
};

template<typename X>
struct Ut
{
  X **t;
};

template<typename I>
void
tfoo (void)
{
  Ut<Tt<St<I> > > *u = new Ut<Tt<St<I> > >;
  Tt<St<I> > *real_t = new Tt<St<int> >;
  St<I> *real_s = new St<int>;
  Tt<St<I> > **t_pp = &real_t;
  St<I> **s_pp = &real_s;
  St<I> ***s_ppp = &s_pp;
  u->t = t_pp;
  (*u->t)->s = s_ppp;
  for (int i = 0; i < 10; i++)
    (**((*u->t)->s))->x[i] = 0;
#pragma omp target map(u->t, *u->t, (*u->t)->s, *(*u->t)->s, **(*u->t)->s, \
		       (**(*u->t)->s)->x[0:10])
  for (int i = 0; i < 10; i++)
    (**((*u->t)->s))->x[i] = i * 3;
  for (int i = 0; i < 10; i++)
    assert ((**((*u->t)->s))->x[i] == i * 3);
  delete real_s;
  delete real_t;
  delete u;
}

int main (int argc, char *argv[])
{
  foo ();
  tfoo<int> ();
  return 0;
}
