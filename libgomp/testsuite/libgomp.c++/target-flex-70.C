/* CTAD in target regions.  */

template<typename T>
struct S
{
  T _v;
};

template<typename T>
S(T) -> S<T>;

bool f()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      S s{42};
      ok = s._v == 42;
    }
  return ok;
}

int main()
{
  return f() ? 0 : 1;
}
