#include <cstring>
#include <cassert>

struct S {
  int *&ptr;
  S(int *&ptr_) : ptr(ptr_) { }
};

int main()
{
  int *orig = new int[100];

  memset (orig, 0, sizeof (int) * 100);

  S svar(orig);

#pragma omp target enter data map(to: svar.ptr, svar.ptr[10:80])

#pragma omp target
  {
    for (int i = 10; i < 90; i++)
      svar.ptr[i]++;
  }

#pragma omp target exit data map(release: svar.ptr) map(from: svar.ptr[10:80])

  for (int i = 0; i < 100; i++)
    assert (i >= 10 && i < 90 && svar.ptr[i] == 1
	    || svar.ptr[i] == 0);

  delete orig;

  return 0;
}
