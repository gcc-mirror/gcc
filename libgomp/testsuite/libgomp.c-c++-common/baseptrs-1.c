#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#define N 32

typedef struct {
  int x2[10][N];
} x1type;

typedef struct {
  x1type x1[10];
} p2type;

typedef struct {
  p2type *p2;
} p1type;

typedef struct {
  p1type *p1;
} x0type;

typedef struct {
  x0type x0[10];
} p0type;

int main(int argc, char *argv[])
{
  p0type *p0;
  int k1 = 0, k2 = 0, k3 = 0, n = N;

  p0 = (p0type *) malloc (sizeof *p0);
  p0->x0[0].p1 = (p1type *) malloc (sizeof *p0->x0[0].p1);
  p0->x0[0].p1->p2 = (p2type *) malloc (sizeof *p0->x0[0].p1->p2);
  memset (p0->x0[0].p1->p2, 0, sizeof *p0->x0[0].p1->p2);

#pragma omp target map(tofrom: p0->x0[k1].p1->p2[k2].x1[k3].x2[4][0:n]) \
		   map(to: p0->x0[k1].p1, p0->x0[k1].p1->p2) \
		   map(to: p0->x0[k1].p1[0])
  {
    for (int i = 0; i < n; i++)
      p0->x0[k1].p1->p2[k2].x1[k3].x2[4][i] = i;
  }

  for (int i = 0; i < n; i++)
    assert (i == p0->x0[k1].p1->p2[k2].x1[k3].x2[4][i]);

  return 0;
}
