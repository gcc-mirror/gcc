// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <chrono>
#include <testsuite_hooks.h>

void
test_format_strings()
{
  using namespace std::chrono_literals;

  // valid format strings
  VERIFY( std::format("{}", 1s) == "1s" );
  VERIFY( std::format("{:}", 1s) == "1s" );
  VERIFY( std::format("{:L}", 1s) == "1s" );
  VERIFY( std::format("{:%%%n%t}", 1s) == "%\n\t" );
  VERIFY( std::format("{:L%%%n%t}", 1s) == "%\n\t" );
  VERIFY( std::format("{:4%%}", 1s) == "%   " );
  VERIFY( std::format("{:4L%%}", 1s) == "%   " );
  VERIFY( std::format("{: >4}", 1s) == "  1s" );
  VERIFY( std::format("{: <4}", 1s) == "1s  " );
  VERIFY( std::format("{: <4L}", 1s) == "1s  " );
  VERIFY( std::format("{: >4%%}", 1s) == "   %" );
  VERIFY( std::format("{: >4L%%}", 1s) == "   %" );
  VERIFY( std::format("{: ^4%%}", 1s) == " %  " );
}

template<typename... Args>
bool
is_format_string_for(const char* str, Args&&... args)
{
  try {
    (void) std::vformat(str, std::make_format_args(args...));
    return true;
  } catch (const std::format_error&) {
    return false;
  }
}

void
test_bad_format_strings()
{
  std::chrono::sys_seconds t{};

  // literal '%' must be formatted as "%%"
  VERIFY( not is_format_string_for("{:%}", t) );

  // chrono-specs must start with '%'
  VERIFY( not is_format_string_for("{:a%}", t) );
  VERIFY( not is_format_string_for("{:La%}", t) );

  // '{' not valid in chrono-specs
  VERIFY( not is_format_string_for("{:%%{{%%}", t) );

  // padding with leading zero not valid for chrono types
  VERIFY( not is_format_string_for("{:04%T}", t) );

  // precision only valid for chrono::duration types with floating-point rep.
  VERIFY( not is_format_string_for("{:.4}", t) );

  // unfinished format string
  VERIFY( not is_format_string_for("{:", t) );

  // dangling modifiers
  VERIFY( not is_format_string_for("{:%E}", t) );
  VERIFY( not is_format_string_for("{:%O}", t) );

  // modifier not valid for conversion specifier
  VERIFY( not is_format_string_for("{:%Ea}", t) );
  VERIFY( not is_format_string_for("{:%Oa}", t) );
}

template<typename I>
struct move_only_iterator
{
  using iterator = I;
  using value_type = iterator::value_type;
  using difference_type = iterator::difference_type;
  using iterator_category = std::output_iterator_tag;

  move_only_iterator(iterator b) : base_(b) { }
  move_only_iterator(move_only_iterator&&) = default;
  move_only_iterator& operator=(move_only_iterator&&) = default;

  move_only_iterator& operator++() { ++base_; return *this; }
  move_only_iterator operator++(int) { auto tmp = *this; ++base_; return tmp; }

  decltype(auto) operator*() { return *base_; }

private:
  iterator base_;
};

void
test_move_only_iterator()
{
  using namespace std::chrono;
  utc_seconds ut(1671543754s);
  sys_seconds st(1671543727s);

  std::string str;
  move_only_iterator mo(std::back_inserter(str));
  std::format_to(std::move(mo), "{:%F} {:%T} {:%Q}", ut, st, 1s);
  VERIFY( str == "2022-12-20 13:42:07 1" );

  std::vector<wchar_t> vec;
  move_only_iterator wmo(std::back_inserter(vec));
  std::format_to(std::move(wmo), L"{:%F} {:%T} {:%Q}", ut, st, 2s);
  VERIFY( std::wstring_view(vec.data(), vec.size()) == L"2022-12-20 13:42:07 2" );
}

int main()
{
  test_format_strings();
  test_bad_format_strings();
  test_move_only_iterator();
}
