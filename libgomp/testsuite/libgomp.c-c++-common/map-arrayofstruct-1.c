#include <stdlib.h>
#include <assert.h>

struct st {
  int *p;
};

int main (void)
{
  struct st s[2];
  s[0].p = (int *) calloc (5, sizeof (int));
  s[1].p = (int *) calloc (5, sizeof (int));

#pragma omp target map(s[0].p, s[1].p, s[0].p[0:2], s[1].p[1:3])
  {
    s[0].p[0] = 5;
    s[1].p[1] = 7;
  }

#pragma omp target map(s, s[0].p[0:2], s[1].p[1:3])
  {
    s[0].p[0]++;
    s[1].p[1]++;
  }

#pragma omp target map(s[0:2], s[0].p[0:2], s[1].p[1:3])
  {
    s[0].p[0]++;
    s[1].p[1]++;
  }

  assert (s[0].p[0] == 7);
  assert (s[1].p[1] == 9);

  free (s[0].p);
  free (s[1].p);
  return 0;
}
