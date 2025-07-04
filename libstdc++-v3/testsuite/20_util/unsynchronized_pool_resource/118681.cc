// { dg-do run { target c++17 } }
// Bug 118681 - unsynchronized_pool_resource may fail to respect alignment

#include <memory_resource>
#include <cstdio>
#include <testsuite_hooks.h>

#ifndef RESOURCE
# define RESOURCE std::pmr::unsynchronized_pool_resource
#endif

bool any_misaligned = false;

bool
is_aligned(void* p, [[maybe_unused]] std::size_t size, std::size_t alignment)
{
  const bool misaligned = reinterpret_cast<std::uintptr_t>(p) % alignment;
#ifdef DEBUG
  std::printf("allocate(%2zu, %2zu): %p is aligned %scorrectly\n",
	      size, alignment, p, misaligned ? "in" : "");
  any_misaligned |= misaligned;
  return true;
#endif
  return ! misaligned;
}

void
test_alignment(std::pmr::memory_resource& res, bool dealloc)
{
  for (std::size_t alignment : { 8, 16, 32, 64 })
  {
    for (std::size_t size : { 9, 12, 24, 40, 48, 56, 72 })
    {
      void* p1 = res.allocate(size, alignment);
      void* p2 = res.allocate(size, alignment);

      VERIFY( is_aligned(p1, size, alignment) );
      VERIFY( is_aligned(p2, size, alignment) );

      if (dealloc)
      {
	res.deallocate(p1, size, alignment);
	res.deallocate(p2, size, alignment);
      }
    }
  }
}

int main()
{
  RESOURCE res;
  test_alignment(res, true);
  res.release();
  test_alignment(res, false);
  res.release();

  VERIFY( ! any_misaligned );
}
