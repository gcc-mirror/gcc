/* PR middle-end/109816  */

/* This variant: without -flto, see target-map-class-2.C for -flto. */

/* iostream.h adds 'globl _ZSt21ios_base_library_initv' with _GLIBCXX_SYMVER_GNU,
   but it shouldn't end up in the offload assembly but only in the host assembly. */

/* Example based on sollve_vv's test_target_data_map_classes.cpp; however,
   relevant is only the 'include' and not the actual executable code.  */

#include <iostream>
#include <omp.h>

using namespace std;

#define N 1000

struct A
{
  int *h_array;
  int size, sum;

  A (int *array, const int s) : h_array(array), size(s), sum(0) { }
  ~A() { h_array = NULL; }
};

void
test_map_tofrom_class_heap ()
{
  int *array = new int[N];
  A *obj = new A (array, N);

  #pragma omp target map(from: array[:N]) map(tofrom: obj[:1])
    {
      int *tmp_h_array = obj->h_array;
      obj->h_array = array;
      int tmp = 0;
      for (int i = 0; i < N; ++i)
	{
	  obj->h_array[i] = 4*i;
	  tmp += 3;
	}
      obj->h_array = tmp_h_array;
      obj->sum = tmp;
    }

  for (int i = 0; i < N; ++i)
    if (obj->h_array[i] != 4*i)
      __builtin_abort ();

  if (3*N != obj->sum)
    {
      std::cout << "sum: " << obj->sum << std::endl;
      __builtin_abort ();
    }

  delete obj;
  delete[] array;
}

void
test_map_tofrom_class_stack ()
{
  int array[N];
  A obj(array, N);

  #pragma omp target map(from: array[:N]) map(tofrom: obj)
    {
      int *tmp_h_array = obj.h_array;
      obj.h_array = array;
      int tmp = 0;
      for (int i = 0; i < N; ++i)
	{
	  obj.h_array[i] = 7*i;
	  tmp += 5;
	}
      obj.h_array = tmp_h_array;
      obj.sum = tmp;
    }

  for (int i = 0; i < N; ++i)
    if (obj.h_array[i] != 7*i)
      __builtin_abort ();

  if (5*N != obj.sum)
    {
      std::cout << "sum: " << obj.sum << std::endl;
      __builtin_abort ();
    }
}

int
main()
{
  test_map_tofrom_class_heap();
  test_map_tofrom_class_stack();
  return 0;
}
