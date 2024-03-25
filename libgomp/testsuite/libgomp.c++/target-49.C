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
  s v_real(la);
  s *v = &v_real;

  memset (la, 0, sizeof la);

  #pragma omp target enter data map(to: v)

  /* Copying the whole v[0] here DOES NOT WORK yet because the reference 'a' is
     not copied "as if" it was mapped explicitly as a member.  FIXME.  */
  #pragma omp target enter data map(to: v[0])

  #pragma omp target
  {
    v->a[5]++;
  }

  #pragma omp target exit data map(release: v[0])
  #pragma omp target exit data map(from: v)

  assert (v->a[5] == 1);

  return 0;
}

// { dg-xfail-run-if "TODO" { offload_device_nonshared_as } }
