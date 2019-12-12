#ifdef __cplusplus
extern "C"
#endif
void abort (void);
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
      arr2[y + z] = arr2[y + z] + 4;
      return &arr[y + z];
    case 3:
      if (z < 0 || z > 60 || (z & 3) || y < 0 || y >= 4)
	abort ();
      #pragma omp atomic
      arr2[y + z] = arr2[y + z] + 16;
      return &arr[y + z];
    case 4:
      if (y != 0 || z > 64 || z <= 0)
	abort ();
      #pragma omp atomic
      arr2[z - 1] = arr2[z - 1] + 64;
      return &arr[z - 1];
    case 5:
      if ((y & 3) != 0 || y < 64 || y >= 96
	  || (z & 127) != 0 || z < 512 || z >= 1024)
	abort ();
      y = (y - 64) + (z - 512) / 128;
      #pragma omp atomic
      arr2[y] = arr2[y] + 256;
      return &arr[y];
    case 6:
      if ((y & 3) != 0 || y <= 64 || y > 96
	  || (z & 127) != 1 || z <= 513 || z > 1025)
	abort ();
      y = (y - 68) + (z - 641) / 128;
      #pragma omp atomic
      arr2[y] = arr2[y] + 1024;
      return &arr[y];
    default:
      abort ();
    }
}

volatile int beg, end, step, step2;
volatile unsigned int begu, endu;

int
main ()
{
  int m;
  beg = 60;
  end = -4;
  step = -4;
  step2 = 4;
  begu = -64U;
  endu = -32U;
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
    #pragma omp task depend (iterator (int *p=&arr3[64]:&arr3[0]:-1) , inout : \
			     foo (4, 0, p - &arr3[0])[0]) depend (in : beg)
    for (i = 0; i < 64; i++)
      if (arr[i] != i + 1)
	abort ();
      else
	arr[i] = arr[i] + 2;
    #pragma omp task depend (iterator (unsigned n=begu:endu:step2, unsigned int o = 512: 1024U: (unsigned char) 128), inout : \
			     foo (5, n + 128, o)[0])
    for (i = 0; i < 64; i++)
      if (arr[i] != i + 3)
	abort ();
      else
	arr[i] = arr[i] + 4;
    #pragma omp task depend (iterator (int unsigned p=endu:begu:step,unsigned q= 1025U:513U:(signed char) -128), in : \
			     foo (6, p + 128, q)[0])
    for (i = 0; i < 64; i++)
      if (arr[i] != i + 7)
	abort ();
      else
	arr[i] = arr[i] + 8;
  }
  for (m = 0; m < 64; m++)
    if (arr[m] != m + 15 || arr2[m] != (m < 32 ? 1365 : 85))
      abort ();
  return 0;
}
