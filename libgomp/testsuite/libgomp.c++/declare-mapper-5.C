// { dg-do run }

#include <cassert>

struct S
{
  int *myarr;
  int len;
};

class C
{
  S smemb;
#pragma omp declare mapper (custom:S s) map(to:s.myarr) \
					map(tofrom:s.myarr[0:s.len])

public:
  C(int l)
  {
    smemb.myarr = new int[l];
    smemb.len = l;
    for (int i = 0; i < l; i++)
      smemb.myarr[i] = 0;
  }
  void bump();
  void check();
};

void
C::bump ()
{
#pragma omp target map(mapper(custom), tofrom: smemb)
  {
    for (int i = 0; i < smemb.len; i++)
      smemb.myarr[i]++;
  }
}

void
C::check ()
{
  for (int i = 0; i < smemb.len; i++)
    assert (smemb.myarr[i] == 1);
}

int main (int argc, char *argv[])
{
  C test (100);
  test.bump ();
  test.check ();
  return 0;
}
