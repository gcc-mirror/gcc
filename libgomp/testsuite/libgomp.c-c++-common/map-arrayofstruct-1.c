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

  /* These mappings not supported by the OpenMP spec, and are currently
     implemented as an extension by GCC for legacy compatibility only.  See
     e.g. OpenMP 5.2, "5.8.3 map Clause":

    "If multiple list items are explicitly mapped on the same construct and
     have the same containing array or have base pointers that share original
     storage, and if any of the list items do not have corresponding list
     items that are present in the device data environment prior to a task
     encountering the construct, then the list items must refer to the same
     array elements of either the containing array or the implicit array of
     the base pointers."
  */

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
