#include <assert.h>
#include <vector>
#include <omp.h>


int
main ()
{
  const omp_alloctrait_t traits[] = { { omp_atk_alignment, 1024 } };

  omp_allocator_handle_t my_alloc;

  my_alloc = omp_init_allocator (omp_default_mem_space, 1, traits);
  assert (my_alloc != omp_null_allocator);

  omp::allocator::user_mem<int> alloc_class(my_alloc);

  std::vector<int, omp::allocator::user_mem<int>> vec1(alloc_class);
  assert (vec1.empty());
  vec1.push_back(1);
  assert (((omp_uintptr_t) &vec1[0]) % 1024 == 0);

  std::vector<int, decltype (alloc_class)> vec2(alloc_class);
  assert (vec2.empty());
  vec2.push_back(1);
  assert (((omp_uintptr_t) &vec2[0]) % 1024 == 0);

  std::vector<int, omp::allocator::user_mem<int>> vec3({omp::allocator::user_mem<int>(my_alloc)});
  assert (vec3.empty());
  vec3.push_back(5);
  assert (((omp_uintptr_t) &vec3[0]) % 1024 == 0);
}
