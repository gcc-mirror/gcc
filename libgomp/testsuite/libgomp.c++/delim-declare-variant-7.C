/* Check that "omp begin declare variant" works on methods in a template
   class declaration.  */

template <typename T>
class test1 {

 private:
  T n;
  static T m;

 public:

  void set_n (T x) { n = x; }
  T get_n (void) { return n; }

  static void set_m (T x) { m = x; }
  static T get_m (void) { return m; }

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  T get_n (void) { return n * 2; }
  static T get_m (void) { return m * 2; }
  #pragma omp end declare variant

  #pragma omp begin declare variant match (construct={target})
  T get_n (void) { return this->n * 2; }
  #pragma omp end declare variant
};

template <typename T>
T test1<T>::m;

int main (void)
{
  test1<int> t1;
  t1.set_n (10);
  if (t1.get_n () != 20) __builtin_abort ();
  test1<int>::set_m (1);
  if (test1<int>::get_m () != 2) __builtin_abort ();
}
