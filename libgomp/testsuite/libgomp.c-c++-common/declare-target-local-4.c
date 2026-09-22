#include <omp.h>
#include <assert.h>

int f (int dev_num)
{
  static int X[1] = { 5 };
  #pragma omp declare target local(X)

  int res;
  #pragma omp target map(from: res) device(dev_num)
  res = ++X[0];

  return res;
}

int main (void)
{
  int cnt;

  cnt = f (omp_initial_device);
  assert (cnt == 6);

  cnt = f (omp_initial_device);
  assert (cnt == 7);

  if (omp_get_num_devices () > 0)
    {
      cnt = f (omp_default_device);
      assert (cnt == 6);
      cnt = f (omp_default_device);
      assert (cnt == 7);
      cnt = f (omp_default_device);
      assert (cnt == 8);
      cnt = f (omp_default_device);
      assert (cnt == 9);

      cnt = f (omp_initial_device);
      assert (cnt == 8);
    }
  else
    {
      cnt = f (omp_default_device);
      assert (cnt == 8);
    }

  return 0;
}
