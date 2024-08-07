/* { dg-do run }  */
/* PR middle-end/116107  */

#include <omp.h>

int arr[15] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
#pragma omp declare target link(arr)

#pragma omp begin declare target
void f(int *res)
{
  __builtin_memcpy (res, &arr[5], sizeof(int)*10);
}

void g(int *res)
{
  __builtin_memcpy (res, &arr[3], sizeof(int)*10);
}
#pragma omp end declare target

int main()
{
  int res[10], res2;
  for (int dev = 0; dev < omp_get_num_devices(); dev++)
    {
      __builtin_memset (res, 0, sizeof (res));
      res2 = 99;

      #pragma omp target enter data map(arr[5:10]) device(dev)

      #pragma omp target map(from: res) device(dev)
	f (res);

      #pragma omp target map(from: res2) device(dev)
	res2 = arr[5];

      if (res2 != 6)
	__builtin_abort ();
      for (int i = 0; i < 10; i++)
	if (res[i] != 6 + i)
	  __builtin_abort ();

      #pragma omp target exit data map(release:arr[5:10]) device(dev)

      for (int i = 0; i < 15; i++)
	arr[i] *= 10;
      __builtin_memset (res, 0, sizeof (res));

      #pragma omp target enter data map(arr[3:10]) device(dev)

      #pragma omp target map(from: res) device(dev)
	g (res);

      for (int i = 0; i < 10; i++)
	if (res[i] != (4 + i)*10)
	  __builtin_abort ();

      for (int i = 0; i < 15; i++) /* Reset. */
	arr[i] /= 10;
    }
  return 0;
}
