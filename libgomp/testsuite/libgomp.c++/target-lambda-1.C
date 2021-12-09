// { dg-do run { target offload_device_nonshared_as } }

#include <cstdlib>
#include <cstring>

template <typename L>
void
omp_target_loop (int begin, int end, L loop)
{
  #pragma omp target teams distribute parallel for
  for (int i = begin; i < end; i++)
    loop (i);
}

struct S
{
  int a, len;
  int *ptr;

  auto merge_data_func (int *iptr, int &b)
  {
    auto fn = [=](void) -> bool
      {
	bool mapped;
	#pragma omp target map(from:mapped)
	{
	  mapped = (ptr != NULL && iptr != NULL);
	  if (mapped)
	    {
	      for (int i = 0; i < len; i++)
		ptr[i] += a + b + iptr[i];
	    }
	}
	return mapped;
      };
    return fn;
  }
};

int x = 1;

int main (void)
{
  const int N = 10;
  int *data1 = new int[N];
  int *data2 = new int[N];
  memset (data1, 0xab, sizeof (int) * N);
  memset (data1, 0xcd, sizeof (int) * N);

  int val = 1;
  int &valref = val;
  #pragma omp target enter data map(alloc: data1[:N], data2[:N])

  omp_target_loop (0, N, [=](int i) { data1[i] = val; });
  omp_target_loop (0, N, [=](int i) { data2[i] = valref + 1; });

  #pragma omp target update from(data1[:N], data2[:N])

  for (int i = 0; i < N; i++)
    {
      if (data1[i] != 1) abort ();
      if (data2[i] != 2) abort ();
    }

  #pragma omp target exit data map(delete: data1[:N], data2[:N])

  int b = 8;
  S s = { 4, N, data1 };
  auto f = s.merge_data_func (data2, b);

  if (f ()) abort ();

  #pragma omp target enter data map(to: data1[:N])
  if (f ()) abort ();

  #pragma omp target enter data map(to: data2[:N])
  if (!f ()) abort ();

  #pragma omp target exit data map(from: data1[:N], data2[:N])

  for (int i = 0; i < N; i++)
    {
      if (data1[i] != 0xf) abort ();
      if (data2[i] != 2) abort ();
    }

  return 0;
}
