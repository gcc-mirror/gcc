// { dg-options "-std=gnu++23 -lstdc++_libbacktrace" }
// { dg-do run { target c++23 } }
// { dg-require-effective-target stacktrace }

#include <stacktrace>
#include <memory>
#include <new>
#include "testsuite_hooks.h"

template<typename T>
struct Allocator
{
  using value_type = T;
  using propagate_on_container_move_assignment = std::true_type;

  explicit
  Allocator(unsigned max = -1u) : max_size(max) { }

  template<typename U>
    Allocator(const Allocator<U>& a) : max_size(a.max_size) { }

  T*
  allocate(std::size_t n)
  {
    if (n > max_size)
      throw std::bad_alloc();

    return std::allocator<T>().allocate(n);
  }

  void
  deallocate(T* p, std::size_t n) noexcept
  {
    std::allocator<T>().deallocate(p, n);
  }

  bool operator==(const Allocator&) const = default;

private:
  unsigned max_size;
};

[[gnu::optimize("O0")]]
void
test_max_depth()
{
  using Stacktrace = std::basic_stacktrace<Allocator<std::stacktrace_entry>>;
  using Alloc = typename Stacktrace::allocator_type;

  [] { [] { [] { [] { [] { [] { [] { [] {
    auto t = Stacktrace::current();
    VERIFY( ! t.empty() );
    const auto n = t.size(); // total number of frames
    t = Stacktrace::current(8);
    VERIFY( t.size() == (n - 8) );
    t = Stacktrace::current(n);
    VERIFY( t.empty() );
    t = Stacktrace::current(n - 2);
    VERIFY( t.size() == 2 );
    t = Stacktrace::current(2, 6);
    VERIFY( t.size() == 6 );
    t = Stacktrace::current(n - 2, 6);
    VERIFY( t.size() == 2 );

    t = Stacktrace::current(Alloc(3));
    // Full stacktrace is larger than 3 frames, so allocation fails:
    VERIFY( t.empty() );
    t = Stacktrace::current(3, Alloc(2));
    // Stacktrace still too large after skipping 3 frames, so allocation fails:
    VERIFY( t.empty() );
    t = Stacktrace::current(0, 3, Alloc(3));
    // Capacity for exactly 3 frames is allocated:
    VERIFY( t.size() == 3 );
    t = Stacktrace::current(2, 4, Alloc(4));
    // Capacity for exactly 4 frames is allocated:
    VERIFY( t.size() == 4 );
    t = Stacktrace::current(0, 4, Alloc(3));
    // Capacity for exactly 4 frames is requested, but allocation fails:
    VERIFY( t.empty() );
  }(); }(); }(); }(); }(); }(); }(); }();
}

int main()
{
  test_max_depth();
}
