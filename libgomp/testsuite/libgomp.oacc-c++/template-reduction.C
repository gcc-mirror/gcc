const int n = 100;

// Check explicit template copy map

template<typename T> T
sum (T array[])
{
   T s = 0;

#pragma acc parallel loop num_gangs (10) gang reduction (+:s) copy (s, array[0:n])
  for (int i = 0; i < n; i++)
    s += array[i];

  return s;
}

// Check implicit template copy map

template<typename T> T
sum ()
{
  T s = 0;
  T array[n];

  for (int i = 0; i < n; i++)
    array[i] = i+1;

#pragma acc parallel loop num_gangs (10) gang reduction (+:s) copy (s)
  for (int i = 0; i < n; i++)
    s += array[i];

  return s;
}

// Check present and async

template<typename T> T
async_sum (T array[])
{
   T s = 0;

#pragma acc parallel loop num_gangs (10) gang async (1) present (array[0:n])
   for (int i = 0; i < n; i++)
     array[i] = i+1;

#pragma acc parallel loop num_gangs (10) gang reduction (+:s) present (array[0:n]) copy (s) async wait (1)
  for (int i = 0; i < n; i++)
    s += array[i];

#pragma acc wait

  return s;
}

// Check present and async and an explicit firstprivate

template<typename T> T
async_sum (int c)
{
   T s = 0;

#pragma acc parallel loop num_gangs (10) gang reduction (+:s) copy(s) firstprivate (c) async wait (1)
  for (int i = 0; i < n; i++)
    s += i+c;

#pragma acc wait

  return s;
}

int
main()
{
  int a[n];
  int result = 0;

  for (int i = 0; i < n; i++)
    {
      a[i] = i+1;
      result += i+1;
    }

  if (sum (a) != result)
    __builtin_abort ();

  if (sum<int> () != result)
    __builtin_abort ();

#pragma acc enter data copyin (a)
  if (async_sum (a) != result)
    __builtin_abort ();

  if (async_sum<int> (1) != result)
    __builtin_abort ();
#pragma acc exit data delete (a)

  return 0;
}
