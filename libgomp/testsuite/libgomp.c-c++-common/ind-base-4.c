// { dg-do run }
// { dg-options "-fopenmp" }

#include <assert.h>
#include <stdlib.h>

typedef struct
{
  int x[10];
} S;

typedef struct
{
  S ***s;
} T;

typedef struct
{
  T **t;
} U;

void
foo (void)
{
  U *u = (U *) malloc (sizeof (U));
  T *real_t = (T *) malloc (sizeof (T));
  S *real_s = (S *) malloc (sizeof (S));
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
  free (real_s);
  free (real_t);
  free (u);
}

int main (int argc, char *argv[])
{
  foo ();
  return 0;
}
