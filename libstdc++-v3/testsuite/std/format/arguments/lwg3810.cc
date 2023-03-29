// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

// LWG 3810. CTAD for std::basic_format_args

#include <format>

auto args_store = std::make_format_args(1,2,3);
std::basic_format_args args = args_store;
static_assert(std::is_same_v<decltype(args), std::format_args>);


template<typename Context>
void foo(std::basic_format_args<Context>);

void
test_ctad()
{
  using std::basic_format_args;
  using std::make_format_args;
  using SomeContext = std::wformat_context;

  // foo(make_format_args<SomeContext>(...)); // won't work
  foo(basic_format_args(make_format_args<SomeContext>(1, 2, 3))); // should work
}
