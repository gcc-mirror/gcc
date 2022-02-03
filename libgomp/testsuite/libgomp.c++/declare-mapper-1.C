// { dg-do run }

#include <cstdlib>
#include <cassert>

#define N 64

struct points
{
  double *x;
  double *y;
  double *z;
  size_t len;
};

#pragma omp declare mapper(points p) map(to:p.x, p.y, p.z) \
				     map(p.x[0:p.len]) \
				     map(p.y[0:p.len]) \
				     map(p.z[0:p.len])

struct shape
{
  points tmp;
  points *pts;
  int metadata[128];
};

#pragma omp declare mapper(shape s) map(tofrom:s.pts, *s.pts) map(alloc:s.tmp)

void
alloc_points (points *pts, size_t sz)
{
  pts->x = new double[sz];
  pts->y = new double[sz];
  pts->z = new double[sz];
  pts->len = sz;
  for (int i = 0; i < sz; i++)
    pts->x[i] = pts->y[i] = pts->z[i] = 0;
}

int main (int argc, char *argv[])
{
  shape myshape;
  points mypts;

  myshape.pts = &mypts;

  alloc_points (&myshape.tmp, N);
  myshape.pts = new points;
  alloc_points (myshape.pts, N);

  #pragma omp target map(myshape)
  {
    for (int i = 0; i < N; i++)
      {
	myshape.pts->x[i]++;
	myshape.pts->y[i]++;
	myshape.pts->z[i]++;
      }
  }

  for (int i = 0; i < N; i++)
    {
      assert (myshape.pts->x[i] == 1);
      assert (myshape.pts->y[i] == 1);
      assert (myshape.pts->z[i] == 1);
    }

  #pragma omp target
  {
    for (int i = 0; i < N; i++)
      {
	myshape.pts->x[i]++;
	myshape.pts->y[i]++;
	myshape.pts->z[i]++;
      }
  }

  for (int i = 0; i < N; i++)
    {
      assert (myshape.pts->x[i] == 2);
      assert (myshape.pts->y[i] == 2);
      assert (myshape.pts->z[i] == 2);
    }

  return 0;
}
