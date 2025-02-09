/* Check that "omp begin declare variant" works on methods in a
   class declaration.  */

class test1 {

 private:
  int n;
  static int m;

 public:

  void set_n (int x) { n = x; }
  int get_n (void) { return n; }

  static void set_m (int x) { m = x; }
  static int get_m (void) { return m; }

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  int get_n (void) { return n * 2; }
  static int get_m (void) { return m * 2; }
  #pragma omp end declare variant

  #pragma omp begin declare variant match (construct={target})
  int get_n (void) { return this->n * 2; }
  #pragma omp end declare variant
};

int test1::m;

int main (void)
{
  test1 t1;
  t1.set_n (10);
  if (t1.get_n () != 20) __builtin_abort ();
  test1::set_m (1);
  if (test1::get_m () != 2) __builtin_abort ();
}
