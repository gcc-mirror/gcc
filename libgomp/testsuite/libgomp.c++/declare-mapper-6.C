// { dg-do run }

#include <cassert>

template <typename T>
void adjust (T param)
{
#pragma omp declare mapper (T x) map(to:x.len, x.base) \
				 map(tofrom:x.base[0:x.len])

#pragma omp target
  for (int i = 0; i < param.len; i++)
    param.base[i]++;
}

struct S {
  int len;
  int *base;
};

int main (int argc, char *argv[])
{
  S a;

  a.len = 100;
  a.base = new int[a.len];

  for (int i = 0; i < a.len; i++)
    a.base[i] = 0;

  adjust (a);

  for (int i = 0; i < a.len; i++)
    assert (a.base[i] == 1);

  return 0;
}
