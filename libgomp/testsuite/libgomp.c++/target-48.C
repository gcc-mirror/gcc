#include <cstring>
#include <cassert>

struct s {
  int (&a)[10];
  s(int (&a0)[10]) : a(a0) {}
};

int
main (int argc, char *argv[])
{
  int la[10];
  s v(la);

  memset (la, 0, sizeof la);

  #pragma omp target enter data map(to: v)

  /* This mapping must use GOMP_MAP_ATTACH_DETACH not GOMP_MAP_ALWAYS_POINTER,
     else the host reference v.a will be corrupted on copy-out.  */

  #pragma omp target map(v.a[0:10])
  {
    v.a[5]++;
  }

  #pragma omp target exit data map(from: v)

  assert (v.a[5] == 1);

  return 0;
}
