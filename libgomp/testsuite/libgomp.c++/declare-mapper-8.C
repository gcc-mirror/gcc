// { dg-do run }

#include <cassert>

struct S
{
  int *myarr;
  int len;
};

template<typename T>
class C
{
  T memb;
#pragma omp declare mapper (T t) map(to:t.len, t.myarr) \
				 map(tofrom:t.myarr[0:t.len])

public:
  C(int sz);
  ~C();
  void bump();
  void check();
};

template<typename T>
C<T>::C(int sz)
{
  memb.myarr = new int[sz];
  for (int i = 0; i < sz; i++)
    memb.myarr[i] = 0;
  memb.len = sz;
}

template<typename T>
C<T>::~C()
{
  delete[] memb.myarr;
}

template<typename T>
void C<T>::bump()
{
#pragma omp target map(memb)
  for (int i = 0; i < memb.len; i++)
    memb.myarr[i]++;
}

template<typename T>
void C<T>::check()
{
  for (int i = 0; i < memb.len; i++)
    assert (memb.myarr[i] == 1);
}

int main(int argc, char *argv[])
{
  C<S> c_int(100);
  c_int.bump();
  c_int.check();
  return 0;
}
