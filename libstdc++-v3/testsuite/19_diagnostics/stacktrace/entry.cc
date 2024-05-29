// { dg-options "-funwind-tables -lstdc++exp" }
// { dg-do run { target c++23 } }
// { dg-require-cpp-feature-test __cpp_lib_stacktrace }

#include <stacktrace>
#include "testsuite_hooks.h"

static_assert( std::regular<std::stacktrace_entry> );
static_assert( std::three_way_comparable<std::stacktrace_entry> );

constexpr bool
test_constexpr()
{
  std::stacktrace_entry empty;
  VERIFY( !empty );
  VERIFY( empty == empty );
  VERIFY( std::is_eq(empty <=> empty) );

  std::stacktrace_entry::native_handle_type native  = empty.native_handle();
  VERIFY( empty.native_handle() == native );

  return true;
}
static_assert( test_constexpr() );

void
test_members()
{
  std::stacktrace_entry empty;
  VERIFY( empty.description().size() == 0 );
  VERIFY( empty.source_file().size() == 0 );
  VERIFY( empty.source_line() == 0 );

  std::stacktrace_entry e1 = std::stacktrace::current().at(0);
  std::stacktrace_entry e2 = std::stacktrace::current().at(0);
  VERIFY( e1 != e2 );
  VERIFY( e1.description() == e2.description() );
  VERIFY( e1.source_file() == e2.source_file() );
  VERIFY( e1.source_line() == (__LINE__ - 5) );
  VERIFY( e2.source_line() == (__LINE__ - 5) );

  std::stacktrace_entry e3 = []{
    return std::stacktrace::current().at(0);
  }();
  VERIFY( e1 != e3 );
  VERIFY( e1.description() != e3.description() );
  VERIFY( e1.source_file() == e3.source_file() );
  VERIFY( e3.source_line() == (__LINE__ - 5) );
}

int main()
{
  test_members();
}
