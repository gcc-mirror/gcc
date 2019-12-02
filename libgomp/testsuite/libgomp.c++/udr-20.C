// PR c++/60228
// { dg-additional-options "-std=c++11" }

extern "C" void abort ();

struct A
{
  typedef int T;
  #pragma omp declare reduction (x : T : omp_out += omp_in + [](){ return 0; }()) initializer (omp_priv = [](){ return 0; }())
  static void foo ();
};

template <typename T>
struct B
{
  #pragma omp declare reduction (x : T : omp_out += omp_in + [](){ return T (0); }()) initializer (omp_priv = [](){ return T (0); }())
  static void foo ();
};

void
A::foo ()
{
  int r = 0, s = 0;
  #pragma omp parallel for reduction (x : r, s)
  for (int i = 0; i < 64; i++)
    {
      r++;
      s += i;
    }
  if (r != 64 || s != (64 * 63) / 2)
    abort ();
}

template <typename T>
void
B<T>::foo ()
{
  T r = 0, s = 0;
  #pragma omp parallel for reduction (x : r, s)
  for (int i = 0; i < 64; i++)
    {
      r++;
      s += i;
    }
  if (r != 64 || s != (64 * 63) / 2)
    abort ();
}

int
main ()
{
  A::foo ();
  B<long>::foo ();
}
