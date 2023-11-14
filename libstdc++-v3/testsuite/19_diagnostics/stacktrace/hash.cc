// { dg-options "-lstdc++exp" }
// { dg-do run { target c++23 } }
// { dg-require-effective-target stacktrace }

#include <stacktrace>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test_hash()
{
  using Alloc = __gnu_test::uneq_allocator<std::stacktrace_entry>;
  using S = std::basic_stacktrace<Alloc>;
  S s;
  std::size_t h = std::hash<S>()(s);
  std::size_t h2 = std::hash<S>()(S::current());
  VERIFY( h != h2 );
}

int main()
{
  test_hash();
}
