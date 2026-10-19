// { dg-do run }

#include <new>
#include <stdint.h>
#include <testsuite_hooks.h>

#pragma GCC diagnostic ignored "-Walloc-size-larger-than="

void
check_aligned_new_overflow(std::size_t size, std::size_t align)
{
#if __cpp_aligned_new
  std::align_val_t alignv = (std::align_val_t)align;
  try
    {
      (void) ::operator new(size, alignv);
      VERIFY(false);
    }
  catch (const std::bad_alloc&)
    {
    }

  void* p = ::operator new(size, alignv, std::nothrow);
  VERIFY(p == 0);
#endif
}

int main()
{
  check_aligned_new_overflow(-9, 32);
  check_aligned_new_overflow(SIZE_MAX, 8);
  check_aligned_new_overflow(SIZE_MAX, 32);
  check_aligned_new_overflow(SIZE_MAX, 1024);
  check_aligned_new_overflow(SIZE_MAX, 65536);
  check_aligned_new_overflow(SIZE_MAX -     1,    16);
  check_aligned_new_overflow(SIZE_MAX -  1025,  1024);
  check_aligned_new_overflow(SIZE_MAX -  1024,  1024);
  check_aligned_new_overflow(SIZE_MAX - 65536, 65536);
}
