// { dg-do run }

#include <cassert>

#define N 256

struct doublebuf
{
  int buf_a[N][N];
  int buf_b[N][N];
};

#pragma omp declare mapper(lo:doublebuf b) map(b.buf_a[0:N/2][0:N]) \
					   map(b.buf_b[0:N/2][0:N])

#pragma omp declare mapper(hi:doublebuf b) map(b.buf_a[N/2:N/2][0:N]) \
					   map(b.buf_b[N/2:N/2][0:N])

int main (int argc, char *argv[])
{
  doublebuf db;

  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      db.buf_a[i][j] = db.buf_b[i][j] = 0;

  #pragma omp target map(mapper(lo), tofrom:db)
  {
    for (int i = 0; i < N / 2; i++)
      for (int j = 0; j < N; j++)
	{
	  db.buf_a[i][j]++;
	  db.buf_b[i][j]++;
	}
  }

  #pragma omp target map(mapper(hi), tofrom:db)
  {
    for (int i = N / 2; i < N; i++)
      for (int j = 0; j < N; j++)
	{
	  db.buf_a[i][j]++;
	  db.buf_b[i][j]++;
	}
  }

  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      {
	assert (db.buf_a[i][j] == 1);
	assert (db.buf_b[i][j] == 1);
      }

  return 0;
}
