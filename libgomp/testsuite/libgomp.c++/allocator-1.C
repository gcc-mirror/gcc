// { dg-do run }

#include <omp.h>
#include <memory>
#include <limits>

template<typename T, template<typename> class Alloc>
void test (T const initial_value = T())
{
  using Allocator = Alloc<T>;
  Allocator a;
  using Traits = std::allocator_traits<Allocator>;
  static_assert (__is_same(typename Traits::allocator_type,     Allocator       ));
  static_assert (__is_same(typename Traits::value_type,         T               ));
  static_assert (__is_same(typename Traits::pointer,            T*              ));
  static_assert (__is_same(typename Traits::const_pointer,      T const*        ));
  static_assert (__is_same(typename Traits::void_pointer,       void*           ));
  static_assert (__is_same(typename Traits::const_void_pointer, void const*     ));
  static_assert (__is_same(typename Traits::difference_type,    __PTRDIFF_TYPE__));
  static_assert (__is_same(typename Traits::size_type,          __SIZE_TYPE__   ));
  static_assert (Traits::propagate_on_container_copy_assignment::value == false);
  static_assert (Traits::propagate_on_container_move_assignment::value == false);
  static_assert (Traits::propagate_on_container_swap::value == false);
  static_assert (Traits::is_always_equal::value == true);

  static constexpr __SIZE_TYPE__ correct_max_size
    = std::numeric_limits<__SIZE_TYPE__>::max () / sizeof (T);
  if (Traits::max_size (a) != correct_max_size)
    __builtin_abort ();

  static constexpr __SIZE_TYPE__ alloc_count = 1;
  T *p = Traits::allocate (a, alloc_count);
  if (p == nullptr)
    __builtin_abort ();
  Traits::construct (a, p, initial_value);
  if (*p != initial_value)
    __builtin_abort ();
  Traits::destroy (a, p);
  Traits::deallocate (a, p, alloc_count);
  /* Not interesting but might as well test it.  */
  static_cast<void>(Traits::select_on_container_copy_construction (a));

  if (!(a == Allocator()))
    __builtin_abort ();
  if (a != Allocator())
    __builtin_abort ();
  if (!(a == Alloc<void>()))
    __builtin_abort ();
  if (a != Alloc<void>())
    __builtin_abort ();
}

#define CHECK_INEQUALITY(other_alloc_templ, type) \
do {									\
  /* Skip tests for itself, those are equal.  Intantiate each  */	\
  /* one with void so we can easily tell if they are the same.  */	\
  if (!__is_same (AllocTempl<void>, other_alloc_templ<void>))		\
    {									\
      other_alloc_templ<type> other;					\
      if (a == other)							\
	__builtin_abort ();						\
      if (!(a != other))						\
	__builtin_abort ();						\
    }									\
} while (false)

template<typename T, template<typename> class AllocTempl>
void test_inequality ()
{
  using Allocator = AllocTempl<T>;
  Allocator a;
  CHECK_INEQUALITY (omp::allocator::null_allocator, void);
  CHECK_INEQUALITY (omp::allocator::default_mem, void);
  CHECK_INEQUALITY (omp::allocator::large_cap_mem, void);
  CHECK_INEQUALITY (omp::allocator::const_mem, void);
  CHECK_INEQUALITY (omp::allocator::high_bw_mem, void);
  CHECK_INEQUALITY (omp::allocator::low_lat_mem, void);
  CHECK_INEQUALITY (omp::allocator::cgroup_mem, void);
  CHECK_INEQUALITY (omp::allocator::pteam_mem, void);
  CHECK_INEQUALITY (omp::allocator::thread_mem, void);
#ifdef __gnu_linux__
  /* Pinning not implemented on other targets.  */
  CHECK_INEQUALITY (ompx::allocator::gnu_pinned_mem, void);
#endif
  /* And again with the same type passed to the allocator.  */
  CHECK_INEQUALITY (omp::allocator::null_allocator, T);
  CHECK_INEQUALITY (omp::allocator::default_mem, T);
  CHECK_INEQUALITY (omp::allocator::large_cap_mem, T);
  CHECK_INEQUALITY (omp::allocator::const_mem, T);
  CHECK_INEQUALITY (omp::allocator::high_bw_mem, T);
  CHECK_INEQUALITY (omp::allocator::low_lat_mem, T);
  CHECK_INEQUALITY (omp::allocator::cgroup_mem, T);
  CHECK_INEQUALITY (omp::allocator::pteam_mem, T);
  CHECK_INEQUALITY (omp::allocator::thread_mem, T);
#ifdef __gnu_linux__
  CHECK_INEQUALITY (ompx::allocator::gnu_pinned_mem, T);
#endif
}

#undef CHECK_INEQUALITY

struct S
{
  int _v0;
  bool _v1;
  float _v2;

  bool operator== (S const& other) const noexcept {
    return _v0 == other._v0
	   && _v1 == other._v1
	   && _v2 == other._v2;
  }
  bool operator!= (S const& other) const noexcept {
    return !this->operator==(other);
  }
};

int main ()
{
  test<int, omp::allocator::null_allocator>(42);
  test<int, omp::allocator::default_mem>(42);
  test<int, omp::allocator::large_cap_mem>(42);
  test<int, omp::allocator::const_mem>(42);
  test<int, omp::allocator::high_bw_mem>(42);
  test<int, omp::allocator::low_lat_mem>(42);
  test<int, omp::allocator::cgroup_mem>(42);
  test<int, omp::allocator::pteam_mem>(42);
  test<int, omp::allocator::thread_mem>(42);
#ifdef __gnu_linux__
  test<int, ompx::allocator::gnu_pinned_mem>(42);
#endif

  test<long long, omp::allocator::null_allocator>(42);
  test<long long, omp::allocator::default_mem>(42);
  test<long long, omp::allocator::large_cap_mem>(42);
  test<long long, omp::allocator::const_mem>(42);
  test<long long, omp::allocator::high_bw_mem>(42);
  test<long long, omp::allocator::low_lat_mem>(42);
  test<long long, omp::allocator::cgroup_mem>(42);
  test<long long, omp::allocator::pteam_mem>(42);
  test<long long, omp::allocator::thread_mem>(42);
#ifdef __gnu_linux__
  test<long long, ompx::allocator::gnu_pinned_mem>(42);
#endif

  test<S, omp::allocator::null_allocator>( S{42, true, 128.f});
  test<S, omp::allocator::default_mem>(    S{42, true, 128.f});
  test<S, omp::allocator::large_cap_mem>(  S{42, true, 128.f});
  test<S, omp::allocator::const_mem>(      S{42, true, 128.f});
  test<S, omp::allocator::high_bw_mem>(    S{42, true, 128.f});
  test<S, omp::allocator::low_lat_mem>(    S{42, true, 128.f});
  test<S, omp::allocator::cgroup_mem>(     S{42, true, 128.f});
  test<S, omp::allocator::pteam_mem>(      S{42, true, 128.f});
  test<S, omp::allocator::thread_mem>(     S{42, true, 128.f});
#ifdef __gnu_linux__
  test<S, ompx::allocator::gnu_pinned_mem>(S{42, true, 128.f});
#endif

  test_inequality<int, omp::allocator::null_allocator>();
  test_inequality<int, omp::allocator::default_mem>();
  test_inequality<int, omp::allocator::large_cap_mem>();
  test_inequality<int, omp::allocator::const_mem>();
  test_inequality<int, omp::allocator::high_bw_mem>();
  test_inequality<int, omp::allocator::low_lat_mem>();
  test_inequality<int, omp::allocator::cgroup_mem>();
  test_inequality<int, omp::allocator::pteam_mem>();
  test_inequality<int, omp::allocator::thread_mem>();
#ifdef __gnu_linux__
  test_inequality<int, ompx::allocator::gnu_pinned_mem>();
#endif
}
