// { dg-do run }
// { dg-additional-options "-Wno-psabi" }

#include <omp.h>
#include <vector>

template<typename T>
bool ptr_is_aligned(T *ptr, std::size_t alignment)
{
  /* ALIGNMENT must be a power of 2.  */
  if ((alignment & (alignment - 1)) != 0)
    __builtin_abort ();
  __UINTPTR_TYPE__ ptr_value
    = reinterpret_cast<__UINTPTR_TYPE__>(static_cast<void*>(ptr));
  return (ptr_value % alignment) == 0;
}

template<typename T, template<typename> class Alloc>
void f (T v0, T v1, T v2, T v3)
{
  std::vector<T, Alloc<T>> vec;
  vec.push_back (v0);
  vec.push_back (v1);
  vec.push_back (v2);
  vec.push_back (v3);
  if (vec.at (0) != v0)
    __builtin_abort ();
  if (vec.at (1) != v1)
    __builtin_abort ();
  if (vec.at (2) != v2)
    __builtin_abort ();
  if (vec.at (3) != v3)
    __builtin_abort ();
  if (!ptr_is_aligned (&vec.at (0), alignof (T)))
    __builtin_abort ();
  if (!ptr_is_aligned (&vec.at (1), alignof (T)))
    __builtin_abort ();
  if (!ptr_is_aligned (&vec.at (2), alignof (T)))
    __builtin_abort ();
  if (!ptr_is_aligned (&vec.at (3), alignof (T)))
    __builtin_abort ();
}

struct S0
{
  int _v0;
  bool _v1;
  float _v2;

  bool operator== (S0 const& other) const noexcept {
    return _v0 == other._v0
	   && _v1 == other._v1
	   && _v2 == other._v2;
  }
  bool operator!= (S0 const& other) const noexcept {
    return !this->operator==(other);
  }
};

struct alignas(128) S1
{
  int _v0;
  bool _v1;
  float _v2;

  bool operator== (S1 const& other) const noexcept {
    return _v0 == other._v0
	   && _v1 == other._v1
	   && _v2 == other._v2;
  }
  bool operator!= (S1 const& other) const noexcept {
    return !this->operator==(other);
  }
};

/* Note: the test for const_mem should be disabled in the future.  */

int main ()
{
  f<int, omp::allocator::null_allocator >(0, 1, 2, 3);
  f<int, omp::allocator::default_mem    >(0, 1, 2, 3);
  f<int, omp::allocator::large_cap_mem  >(0, 1, 2, 3);
  f<int, omp::allocator::const_mem      >(0, 1, 2, 3);
  f<int, omp::allocator::high_bw_mem    >(0, 1, 2, 3);
  f<int, omp::allocator::low_lat_mem    >(0, 1, 2, 3);
  f<int, omp::allocator::cgroup_mem     >(0, 1, 2, 3);
  f<int, omp::allocator::pteam_mem      >(0, 1, 2, 3);
  f<int, omp::allocator::thread_mem     >(0, 1, 2, 3);
#ifdef __gnu_linux__
  /* Pinning not implemented on other targets.  */
  f<int, ompx::allocator::gnu_pinned_mem>(0, 1, 2, 3);
#endif

  f<long long, omp::allocator::null_allocator >(0, 1, 2, 3);
  f<long long, omp::allocator::default_mem    >(0, 1, 2, 3);
  f<long long, omp::allocator::large_cap_mem  >(0, 1, 2, 3);
  f<long long, omp::allocator::const_mem      >(0, 1, 2, 3);
  f<long long, omp::allocator::high_bw_mem    >(0, 1, 2, 3);
  f<long long, omp::allocator::low_lat_mem    >(0, 1, 2, 3);
  f<long long, omp::allocator::cgroup_mem     >(0, 1, 2, 3);
  f<long long, omp::allocator::pteam_mem      >(0, 1, 2, 3);
  f<long long, omp::allocator::thread_mem     >(0, 1, 2, 3);
#ifdef __gnu_linux__
  f<long long, ompx::allocator::gnu_pinned_mem>(0, 1, 2, 3);
#endif

  S0 s0_0{   42, true,  111128.f};
  S0 s0_1{  142, false,  11128.f};
  S0 s0_2{ 1142, true,    1128.f};
  S0 s0_3{11142, false,    128.f};
  f<S0, omp::allocator::null_allocator >(s0_0, s0_1, s0_2, s0_3);
  f<S0, omp::allocator::default_mem    >(s0_0, s0_1, s0_2, s0_3);
  f<S0, omp::allocator::large_cap_mem  >(s0_0, s0_1, s0_2, s0_3);
  f<S0, omp::allocator::const_mem      >(s0_0, s0_1, s0_2, s0_3);
  f<S0, omp::allocator::high_bw_mem    >(s0_0, s0_1, s0_2, s0_3);
  f<S0, omp::allocator::low_lat_mem    >(s0_0, s0_1, s0_2, s0_3);
  f<S0, omp::allocator::cgroup_mem     >(s0_0, s0_1, s0_2, s0_3);
  f<S0, omp::allocator::pteam_mem      >(s0_0, s0_1, s0_2, s0_3);
  f<S0, omp::allocator::thread_mem     >(s0_0, s0_1, s0_2, s0_3);
#ifdef __gnu_linux__
  f<S0, ompx::allocator::gnu_pinned_mem>(s0_0, s0_1, s0_2, s0_3);
#endif

  S1 s1_0{   42, true,  111128.f};
  S1 s1_1{  142, false,  11128.f};
  S1 s1_2{ 1142, true,    1128.f};
  S1 s1_3{11142, false,    128.f};

  f<S1, omp::allocator::null_allocator >(s1_0, s1_1, s1_2, s1_3);
  f<S1, omp::allocator::default_mem    >(s1_0, s1_1, s1_2, s1_3);
  f<S1, omp::allocator::large_cap_mem  >(s1_0, s1_1, s1_2, s1_3);
  f<S1, omp::allocator::const_mem      >(s1_0, s1_1, s1_2, s1_3);
  f<S1, omp::allocator::high_bw_mem    >(s1_0, s1_1, s1_2, s1_3);
  f<S1, omp::allocator::low_lat_mem    >(s1_0, s1_1, s1_2, s1_3);
  f<S1, omp::allocator::cgroup_mem     >(s1_0, s1_1, s1_2, s1_3);
  f<S1, omp::allocator::pteam_mem      >(s1_0, s1_1, s1_2, s1_3);
  f<S1, omp::allocator::thread_mem     >(s1_0, s1_1, s1_2, s1_3);
#ifdef __gnu_linux__
  f<S1, ompx::allocator::gnu_pinned_mem>(s1_0, s1_1, s1_2, s1_3);
#endif
}
