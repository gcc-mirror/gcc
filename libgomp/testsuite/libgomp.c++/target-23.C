extern "C" void abort ();

struct S
{
  int *data;
};

int
main (void)
{
  #define SZ 10
  S *s = new S ();
  s->data = new int[SZ];

  for (int i = 0; i < SZ; i++)
    s->data[i] = 0;

  #pragma omp target enter data map(to: s)
  #pragma omp target enter data map(to: s->data, s->data[:SZ])
  #pragma omp target
  {
    for (int i = 0; i < SZ; i++)
      s->data[i] = i;
  }
  #pragma omp target exit data map(from: s->data, s->data[:SZ])
  #pragma omp target exit data map(from: s)

  for (int i = 0; i < SZ; i++)
    if (s->data[i] != i)
      abort ();

  return 0;
}

