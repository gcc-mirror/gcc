#include <assert.h>

#define C 55

int i, j, k;

static void
test_bzero (unsigned size)
{
  unsigned bsize = size * sizeof (int);
  int *x = __builtin_malloc (bsize);
  __builtin_memset (x, C, bsize);

#pragma omp target map(tofrom: x[:size]) map(from: bsize)
  {
    __builtin_bzero (x, bsize);
  }

  char *buffer = (char *) x;
  for (unsigned i = 0; i < bsize; ++i)
    assert (buffer[i] == 0);
}

static void
test_memcpy (unsigned size)
{
  unsigned bsize = size * sizeof (int);
  int *x = __builtin_malloc (bsize);
  __builtin_memset (x, C, bsize);
  int *y = __builtin_malloc (bsize);

#pragma omp target map(tofrom: x[:size], y[:size]) map(from: bsize)
  {
    __builtin_memcpy (y, x, bsize);
  }

  char *buffer = (char *) y;
  for (unsigned i = 0; i < bsize; ++i)
    assert (buffer[i] == C);
}

static void
test_mempcpy (unsigned size)
{
  unsigned bsize = size * sizeof (int);
  int *x = __builtin_malloc (bsize);
  __builtin_memset (x, C, bsize);
  int *y = __builtin_malloc (bsize);
  int *ptr = 0;

#pragma omp target map(tofrom :x[:size], y[:size], ptr) map(from: bsize)
  {
    ptr = __builtin_mempcpy (y, x, bsize);
  }

  char *buffer = (char *) y;
  for (unsigned i = 0; i < bsize; ++i)
    assert (buffer[i] == C);

  assert (ptr == y + size);
}

static void
test_memset (unsigned size)
{
  unsigned bsize = size * sizeof (int);
  int *x = __builtin_malloc (bsize);
  __builtin_bzero (x, bsize);

#pragma omp target map(tofrom : x[:size]) map(from: bsize)
  {
    __builtin_memset (x, C, bsize);
  }

  char *buffer = (char *) x;
  for (unsigned i = 0; i < bsize; ++i)
    assert (buffer[i] == C);
}

int
main (void)
{
  unsigned tests[] = {1, 2, 3, 4, 5, 8, 15, 17, 23, 33, 0};

  for (unsigned i = 0; tests[i]; i++)
    {
      test_bzero (tests[i]);
      test_memset (tests[i]);
      test_memcpy (tests[i]);
      test_mempcpy (tests[i]);
    }
}
