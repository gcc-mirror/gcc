// { dg-do run }
// { dg-additional-options "-std=c++20" }
// { dg-additional-options -DMEM_SHARED { target offload_device_shared_as } }

#include <stdlib.h>
#include <time.h>
#include <span>

#define N 64

void init (int data[])
{
  for (int i = 0; i < N; ++i)
    data[i] = rand ();
}

#pragma omp declare target
bool validate (const std::span<int, N> &span, int data[])
{
  for (int i = 0; i < N; ++i)
    if (span[i] != data[i] * data[i])
      return false;
  return true;
}
#pragma omp end declare target

int main (void)
{
  int data[N];
  bool ok;
  int elements[N];
  std::span<int, N> span(elements);

  srand (time (NULL));
  init (data);

#ifndef MEM_SHARED
  #pragma omp target enter data map (to: data[:N]) map (alloc: elements, span)
#endif

  #pragma omp target
    {
#ifndef MEM_SHARED
      new (&span) std::span<int, N> (elements);
#endif
      std::copy (data, data + N, span.begin ());
    }

  #pragma omp target teams distribute parallel for
    for (int i = 0; i < N; ++i)
      span[i] *= span[i];

  #pragma omp target map (from: ok)
    {
      ok = validate (span, data);
#ifndef MEM_SHARED
      span.~span ();
#endif
    }

#ifndef MEM_SHARED
  #pragma omp target exit data map (release: elements, span)
#endif

  return ok ? 0 : 1;
}
