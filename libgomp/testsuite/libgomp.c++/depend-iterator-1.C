extern "C" void abort ();
int arr[64], arr2[64], arr3[64];

int *
foo (int x, int y, long z)
{
  int v;
  switch (x)
    {
    case 1:
      if (z != 0 || y < 0 || y >= 64)
	abort ();
      #pragma omp atomic capture
      {
	v = arr2[y];
	arr2[y]++;
      }
      if (v != 0) abort ();
      return &arr[y];
    case 2:
      if (y < 0 || y > 60 || (y & 3) || z < 0 || z >= 4)
	abort ();
      #pragma omp atomic
      arr2[y + z] = arr2[y + z] + 2;
      return &arr[y + z];
    case 3:
      if (z < 0 || z > 60 || (z & 3) || y < 0 || y >= 4)
	abort ();
      #pragma omp atomic
      arr2[y + z] = arr2[y + z] + 4;
      return &arr[y + z];
    case 4:
      if (y != 0 || z > 64 || z <= 0)
	abort ();
      #pragma omp atomic
      arr2[z - 1] = arr2[z - 1] + 8;
      return &arr[z - 1];
    default:
      abort ();
    }
}

volatile int beg, end, step;

template <int N>
void
bar ()
{
  #pragma omp parallel
  #pragma omp master
  {
    int i;
    for (i = 0; i < 64; i++)
      #pragma omp task depend (iterator (j=i:i+1) , out : foo (1, j, 0)[0])
	arr[i] = i;
    #pragma omp task depend (iterator (int k=beg:end:step,long int l=0:4:1) , inout : \
			     foo (2, k, l)[0], foo (3, l, k)[0]) private (i)
    for (i = 0; i < 64; i++)
      if (arr[i] != i)
	abort ();
      else
	arr[i] = arr[i] + 1;
    #pragma omp task depend (iterator (int *p=&arr3[64]:&arr3[0]:-1), in : \
			     foo (4, 0, p - &arr3[0])[0]) depend (in : beg)
    for (i = 0; i < 64; i++)
      if (arr[i] != i + 1)
	abort ();
      else
	arr[i] = arr[i] + 2;
  }
}

template <typename A, typename B, typename C>
void
baz (A beg, A end, A step)
{
  #pragma omp parallel
  #pragma omp master
  {
    int i;
    for (i = 0; i < 64; i++)
      #pragma omp task depend (iterator (A j=i:i+1),out : foo (1, j, 0)[0])
	arr[i] = i;
    #pragma omp task depend (iterator (A k=beg:end:step,B l=0:4:1), inout : \
			     foo (2, k, l)[0], foo (3, l, k)[0]) private (i)
    for (i = 0; i < 64; i++)
      if (arr[i] != i)
	abort ();
      else
	arr[i] = arr[i] + 1;
    #pragma omp task depend (iterator (C p=&arr3[64]:&arr3[0]:-1), in : \
			     foo (4, 0, p - &arr3[0])[0]) depend (in : beg)
    for (i = 0; i < 64; i++)
      if (arr[i] != i + 1)
	abort ();
      else
	arr[i] = arr[i] + 2;
  }
}

int
main ()
{
  int m;
  beg = 60;
  end = -4;
  step = -4;
  bar<0> ();
  for (m = 0; m < 64; m++)
    if (arr[m] != m + 3 || arr2[m] != 15)
      abort ();
    else
      arr[m] = arr2[m] = 0;
  baz<int, long int, int *> (beg, end, step);
  for (m = 0; m < 64; m++)
    if (arr[m] != m + 3 || arr2[m] != 15)
      abort ();
  return 0;
}
