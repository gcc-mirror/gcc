// We use 'auto' without a function return type, so specify dialect here
// { dg-additional-options "-std=c++14 -fdump-tree-gimple" }
#include <cstdlib>
#include <cstring>
#include <omp.h>

template <typename L>
void
omp_target_loop (int begin, int end, L loop, int dev)
{
  #pragma omp target teams distribute parallel for device(dev)
  for (int i = begin; i < end; i++)
    loop (i);
}

struct S
{
  int a, len;
  int *ptr;

  auto merge_data_func (int *iptr, int &b, int dev)
  {
    auto fn = [=](void) -> bool
      {
	bool mapped = (omp_target_is_present (iptr, dev)
                       && omp_target_is_present (ptr, dev));
	#pragma omp target device(dev)
	{
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

void run (int dev)
{
  const int N = 10;
  int *data1 = new int[N];
  int *data2 = new int[N];
  memset (data1, 0xab, sizeof (int) * N);
  memset (data2, 0xcd, sizeof (int) * N);

  bool shared_mem = (omp_target_is_present (data1, dev)
		     && omp_target_is_present (data2, dev));
  int val = 1;
  int &valref = val;
  #pragma omp target enter data map(alloc: data1[:N], data2[:N]) device(dev)

  omp_target_loop (0, N, [=](int i) { data1[i] = val; }, dev);
  omp_target_loop (0, N, [=](int i) { data2[i] = valref + 1; }, dev);

  #pragma omp target update from(data1[:N], data2[:N]) device(dev)

  for (int i = 0; i < N; i++)
    {
      if (data1[i] != 1) abort ();
      if (data2[i] != 2) abort ();
    }

  #pragma omp target exit data map(delete: data1[:N], data2[:N]) device(dev)

  int b = 8;
  S s = { 4, N, data1 };
  auto f = s.merge_data_func (data2, b, dev);
  if (f () ^ shared_mem) abort ();

  #pragma omp target enter data map(to: data1[:N]) device(dev)
  if (f () ^ shared_mem) abort ();

  #pragma omp target enter data map(to: data2[:N]) device(dev)
  if (!f ()) abort ();

  #pragma omp target exit data map(from: data1[:N], data2[:N]) device(dev)

  for (int i = 0; i < N; i++)
    {
      if ((!shared_mem && data1[i] != 0xf)
	  || (shared_mem && data1[i] != 0x2b))
	abort ();
      if (data2[i] != 2) abort ();
    }
  delete [] data1;
  delete [] data2;
}

int main ()
{
  for (int dev = 0; dev <= omp_get_num_devices (); dev++)
    run (dev);
}

/* { dg-final { scan-tree-dump {#pragma omp target num_teams.* firstprivate\(b\) map\(alloc:MEM.* \[len: 0\]\) map\(firstprivate:this \[pointer assign, bias: 0\]\) map\(alloc:MEM.* \[len: 0\]\) map\(firstprivate:iptr \[pointer assign, bias: 0\]\) firstprivate\(mapped\) map\(to:\*__closure \[len: [0-9]+\]\) map\(firstprivate:__closure \[pointer assign, bias: 0\]\) map\(tofrom:\*_[0-9]+ \[len: [0-9]+\]\) map\(always_pointer:__closure->__this \[pointer assign, bias: 0\]\) map\(alloc:\*_[0-9]+ \[len: 0\]\) map\(alloc:\*_[0-9]+ \[len: 0\]\) device\(_[0-9]+\) map\(attach_zero_length_array_section:__closure->__iptr \[bias: 0\]\) map\(attach_zero_length_array_section:_[0-9]+->ptr \[bias: 0\]\)} "gimple" } } */

/* { dg-final { scan-tree-dump {#pragma omp target num_teams.* firstprivate\(end\) firstprivate\(begin\) map\(to:loop \[len: [0-9]+\]\) map\(alloc:\*_[0-9]+ \[len: 0\]\) device\(dev.[0-9_]+\) map\(attach_zero_length_array_section:loop\.__data1 \[bias: 0\]\)} "gimple" } } */

/* { dg-final { scan-tree-dump {#pragma omp target num_teams.* firstprivate\(end\) firstprivate\(begin\) map\(to:loop \[len: [0-9]+\]\) map\(alloc:\*_[0-9]+ \[len: 0\]\) device\(dev.[0-9_]+\) map\(attach_zero_length_array_section:loop\.__data2 \[bias: 0\]\)} "gimple" } } */
