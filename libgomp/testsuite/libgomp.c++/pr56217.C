// PR middle-end/56217
// { dg-do run }
// { dg-options "-std=c++0x" }

extern "C" void abort ();

template <typename T>
struct ptr {
  T *p;
  ptr () : p () {}
  ptr (ptr &) = delete;
  ptr (ptr &&o) : p(o) {}
  operator T * () { return p; }
};

int a[6] = { 100, 101, 102, 103, 104, 105 };

static ptr<int>
f ()
{
  ptr<int> pt;
  #pragma omp task shared (pt)
    pt.p = a + 2;
  #pragma omp taskwait
  return pt;
}

int
main ()
{
  ptr<int> pt;
  #pragma omp parallel
  #pragma omp single
  if (f () != a + 2 || *f () != 102)
    abort ();
}
