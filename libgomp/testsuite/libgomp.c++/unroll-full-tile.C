template <int dim0, int dim1>
int sum ()
{
  int sum = 0;
  #pragma omp unroll full
  #pragma omp tile sizes (dim0, dim1)
  for (unsigned i = 0; i < 4; i++)
    for (unsigned j = 0; j < 5; j++)
      sum++;

  return sum;
}

int main ()
{
  if (sum <1,1> () != 20)
    __builtin_abort ();
  if (sum <1,2> () != 20)
    __builtin_abort ();
  if (sum <1,3> () != 20)
    __builtin_abort ();
  if (sum <1,4> () != 20)
    __builtin_abort ();
  if (sum <1,5> () != 20)
    __builtin_abort ();

  if (sum <2,1> () != 20)
    __builtin_abort ();
  if (sum <2,2> () != 20)
    __builtin_abort ();
  if (sum <2,3> () != 20)
    __builtin_abort ();
  if (sum <2,4> () != 20)
    __builtin_abort ();
  if (sum <2,5> () != 20)
    __builtin_abort ();

  if (sum <3,1> () != 20)
    __builtin_abort ();
  if (sum <3,2> () != 20)
    __builtin_abort ();
  if (sum <3,3> () != 20)
    __builtin_abort ();
  if (sum <3,4> () != 20)
    __builtin_abort ();
  if (sum <3,5> () != 20)
    __builtin_abort ();

  if (sum <4,1> () != 20)
    __builtin_abort ();
  if (sum <4,2> () != 20)
    __builtin_abort ();
  if (sum <4,3> () != 20)
    __builtin_abort ();
  if (sum <4,4> () != 20)
    __builtin_abort ();
  if (sum <4,5> () != 20)
    __builtin_abort ();

  if (sum <5,1> () != 20)
    __builtin_abort ();
  if (sum <5,2> () != 20)
    __builtin_abort ();
  if (sum <5,3> () != 20)
    __builtin_abort ();
  if (sum <5,4> () != 20)
    __builtin_abort ();
  if (sum <5,5> () != 20)
    __builtin_abort ();

  if (sum <6,1> () != 20)
    __builtin_abort ();
  if (sum <6,2> () != 20)
    __builtin_abort ();
  if (sum <6,3> () != 20)
    __builtin_abort ();
  if (sum <6,4> () != 20)
    __builtin_abort ();
  if (sum <6,5> () != 20)
    __builtin_abort ();
}
