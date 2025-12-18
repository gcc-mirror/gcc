// { dg-do run }

#include <algorithm>
#include <new>
#include <cstdlib>
#include <cstdio>

#if __cplusplus < 201103L
# define NOEXCEPT throw()
# define nullptr 0
# define THROW_BAD_ALLOC throw(std::bad_alloc)
#else
# define NOEXCEPT noexcept
# define THROW_BAD_ALLOC noexcept(false)
#endif

std::size_t limit = -1u;

void* operator new(std::size_t n, const std::nothrow_t&) NOEXCEPT
{
  if (n > limit)
    return NULL;
  return std::malloc(n);
}

void* operator new(std::size_t n) THROW_BAD_ALLOC
{
  return std::malloc(n);
}

void operator delete(void* p) NOEXCEPT { std::free(p); }
#ifdef __cpp_sized_deallocation
void operator delete(void* p, std::size_t) NOEXCEPT { std::free(p); }
#endif

void
test_stl_algo()
{
  int a[] = { 4, 6, 2, 1, 7, 9, 2, 2, 2, 8 };
  limit = -1u;
  std::stable_sort(a, a+10); // gets as much memory as it needs
  limit = 2 * sizeof(int);
  std::stable_sort(a, a+10); // only gets memory for two ints
  limit = 0;
  std::stable_sort(a, a+10); // gets no memory
}

void
test_ranges_algo()
{
#if __cplusplus >= 202002L
  int a[] = { 4, 6, 2, 1, 7, 9, 2, 2, 2, 8 };
  limit = -1u;
  std::ranges::stable_sort(a); // gets as much memory as it needs
  limit = 2 * sizeof(int);
  std::ranges::stable_sort(a); // only gets memory for two ints
  limit = 0;
  std::ranges::stable_sort(a); // gets no memory
#endif
}

int main()
{
  test_stl_algo();
  test_ranges_algo();
}
