// { dg-do run }
// { dg-options "-fopenmp" }

#include <cassert>

struct S
{
  int x[10];
};

struct T
{
  struct S *s;
};

struct U
{
  struct T *t;
};

void
foo_siblist (void)
{
  U *u = new U;
  u->t = new T;
  u->t->s = new S;
  for (int i = 0; i < 10; i++)
    u->t->s->x[i] = 0;
#pragma omp target map(u->t, *(u->t), u->t->s, *u->t->s)
  for (int i = 0; i < 10; i++)
    u->t->s->x[i] = i * 3;
  for (int i = 0; i < 10; i++)
    assert (u->t->s->x[i] == i * 3);
  delete u->t->s;
  delete u->t;
  delete u;
}

void
foo (void)
{
  U *u = new U;
  u->t = new T;
  u->t->s = new S;
  for (int i = 0; i < 10; i++)
    u->t->s->x[i] = 0;
#pragma omp target map(*u, u->t, *(u->t), u->t->s, *u->t->s)
  for (int i = 0; i < 10; i++)
    u->t->s->x[i] = i * 3;
  for (int i = 0; i < 10; i++)
    assert (u->t->s->x[i] == i * 3);
  delete u->t->s;
  delete u->t;
  delete u;
}

void
foo_tofrom (void)
{
  U *u = new U;
  u->t = new T;
  u->t->s = new S;
  for (int i = 0; i < 10; i++)
    u->t->s->x[i] = 0;
#pragma omp target map(u, *u, u->t, *(u->t), u->t->s, *u->t->s)
  for (int i = 0; i < 10; i++)
    u->t->s->x[i] = i * 3;
  for (int i = 0; i < 10; i++)
    assert (u->t->s->x[i] == i * 3);
  delete u->t->s;
  delete u->t;
  delete u;
}

void
bar (void)
{
  U *u = new U;
  U **up = &u;
  u->t = new T;
  u->t->s = new S;
  for (int i = 0; i < 10; i++)
    (*up)->t->s->x[i] = 0;
#pragma omp target map(*up, (*up)->t, *(*up)->t, (*up)->t->s, *(*up)->t->s)
  for (int i = 0; i < 10; i++)
    (*up)->t->s->x[i] = i * 3;
  for (int i = 0; i < 10; i++)
    assert ((*up)->t->s->x[i] == i * 3);
  delete u->t->s;
  delete u->t;
  delete u;
}

void
bar_pp (void)
{
  U *u = new U;
  U **up = &u;
  u->t = new T;
  u->t->s = new S;
  for (int i = 0; i < 10; i++)
    (*up)->t->s->x[i] = 0;
#pragma omp target map(*up, **up, (*up)->t, *(*up)->t, (*up)->t->s, *(*up)->t->s)
  for (int i = 0; i < 10; i++)
    (*up)->t->s->x[i] = i * 3;
  for (int i = 0; i < 10; i++)
    assert ((*up)->t->s->x[i] == i * 3);
  delete u->t->s;
  delete u->t;
  delete u;
}

void
bar_tofrom (void)
{
  U *u = new U;
  U **up = &u;
  u->t = new T;
  u->t->s = new S;
  for (int i = 0; i < 10; i++)
    (*up)->t->s->x[i] = 0;
#pragma omp target map(*up, up, (*up)->t, *(*up)->t, (*up)->t->s, *(*up)->t->s)
  for (int i = 0; i < 10; i++)
    (*up)->t->s->x[i] = i * 3;
  for (int i = 0; i < 10; i++)
    assert ((*up)->t->s->x[i] == i * 3);
  delete u->t->s;
  delete u->t;
  delete u;
}

void
bar_tofrom_pp (void)
{
  U *u = new U;
  U **up = &u;
  u->t = new T;
  u->t->s = new S;
  for (int i = 0; i < 10; i++)
    (*up)->t->s->x[i] = 0;
#pragma omp target map(**up, *up, up, (*up)->t, *(*up)->t, (*up)->t->s, \
		       *(*up)->t->s)
  for (int i = 0; i < 10; i++)
    (*up)->t->s->x[i] = i * 3;
  for (int i = 0; i < 10; i++)
    assert ((*up)->t->s->x[i] == i * 3);
  delete u->t->s;
  delete u->t;
  delete u;
}

int main (int argc, char *argv[])
{
  foo_siblist ();
  foo ();
  foo_tofrom ();
  bar ();
  bar_pp ();
  bar_tofrom ();
  bar_tofrom_pp ();
  return 0;
}
