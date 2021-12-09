extern "C" void abort ();

struct S
{
  int a, b, c, d;

  int sum (void)
  {
    int val = 0;
    val += a + b + this->c + this->d;
    return val;
  }

  int sum_offload (void)
  {
    int val = 0;
    #pragma omp target map(val)
    val += a + b + this->c + this->d;
    return val;
  }
};

int main (void)
{
  S s = { 1, 2, 3, 4 };
  if (s.sum () != s.sum_offload ())
    abort ();
  return 0;
}
