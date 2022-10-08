#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define N 32

typedef struct {
  int arr[N];
  int *ptr;
} sc;

typedef struct {
  sc *c;
} sb;

typedef struct {
  sb *b;
  sc *c;
} sa;

int main (int argc, char *argv[])
{
  sa *p;

  p = (sa *) malloc (sizeof *p);
  p->b = (sb *) malloc (sizeof *p->b);
  p->b->c = (sc *) malloc (sizeof *p->b->c);
  p->c = (sc *) malloc (sizeof *p->c);
  p->b->c->ptr = (int *) malloc (N * sizeof (int));
  p->c->ptr = (int *) malloc (N * sizeof (int));

  for (int i = 0; i < N; i++)
    {
      p->b->c->ptr[i] = 0;
      p->c->ptr[i] = 0;
      p->b->c->arr[i] = 0;
      p->c->arr[i] = 0;
    }

#pragma omp target map(to: p->b, p->b[0], p->c, p->c[0], p->b->c, p->b->c[0]) \
		   map(to: p->b->c->ptr, p->c->ptr) \
		   map(tofrom: p->b->c->ptr[:N], p->c->ptr[:N])
  {
    for (int i = 0; i < N; i++)
      {
	p->b->c->ptr[i] = i;
	p->c->ptr[i] = i * 2;
      }
  }

#pragma omp target map(to: p->b, p->b[0], p->b->c, p->c) \
		   map(tofrom: p->c[0], p->b->c[0])
  {
    for (int i = 0; i < N; i++)
      {
	p->b->c->arr[i] = i * 3;
	p->c->arr[i] = i * 4;
      }
  }

  for (int i = 0; i < N; i++)
    {
      assert (p->b->c->ptr[i] == i);
      assert (p->c->ptr[i] == i * 2);
      assert (p->b->c->arr[i] == i * 3);
      assert (p->c->arr[i] == i * 4);
    }

  return 0;
}
