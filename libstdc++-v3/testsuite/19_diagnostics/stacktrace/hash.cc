// { dg-options "-funwind-tables -lstdc++exp" }
// { dg-do run { target c++23 } }
// { dg-require-cpp-feature-test __cpp_lib_stacktrace }

#include <stacktrace>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test_hash()
{
  using Alloc = __gnu_test::uneq_allocator<std::stacktrace_entry>;
  using S = std::basic_stacktrace<Alloc>;
  S s;
  S cur = S::current();
  std::size_t h = std::hash<S>()(s);
  std::size_t h2 = std::hash<S>()(cur);
  VERIFY( cur.empty() == (h == h2) );
}

int main()
{
  test_hash();
}
