// { dg-do run { target c++23 } }

#include <array>
#include <format>
#include <list>
#include <ranges>
#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <vector>

struct NotFormattable
{};

static_assert(!std::formattable<std::vector<NotFormattable>, char>);
static_assert(!std::formattable<std::span<NotFormattable>, wchar_t>);

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

template<typename... Args>
bool
is_format_string_for(const wchar_t* str, Args&&... args)
{
  try {
    (void) std::vformat(str, std::make_wformat_args(args...));
    return true;
  } catch (const std::format_error&) {
    return false;
  }
}

template<typename Rg, typename CharT>
bool is_range_formatter_spec_for(CharT const* spec, Rg&& rg)
{
  using V = std::remove_cvref_t<std::ranges::range_reference_t<Rg>>;
  std::range_formatter<V, CharT> fmt;
  std::basic_format_parse_context<CharT> pc(spec);
  try {
    (void)fmt.parse(pc);
    return true;
  } catch (const std::format_error&) {
    return false;
  }
}

void
test_format_string()
{
  // invalid format spec 'p'
  VERIFY( !is_range_formatter_spec_for("p", std::vector<int>()) );
  VERIFY( !is_format_string_for("{:p}", std::vector<int>()) );
  VERIFY( !is_range_formatter_spec_for("np", std::vector<int>()) );
  VERIFY( !is_format_string_for("{:np}", std::vector<int>()) );

  // width needs to be integer type
  VERIFY( !is_format_string_for("{:{}}", std::vector<int>(), 1.0f) );

  // element format needs to be valid
  VERIFY( !is_range_formatter_spec_for(":p", std::vector<int>()) );
  VERIFY( !is_format_string_for("{::p}", std::vector<int>()) );
  VERIFY( !is_range_formatter_spec_for("n:p", std::vector<int>()) );
  VERIFY( !is_format_string_for("{:n:p}", std::vector<int>()) );
}

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

template<typename _CharT, typename Range, typename Storage>
void test_output()
{
  using Sv = std::basic_string_view<_CharT>;
  using T = std::ranges::range_value_t<Range>;
  auto makeRange = [](Storage& s) -> Range {
    if constexpr (std::is_same_v<std::remove_cvref_t<Range>, Storage>)
      return s;
    else
      return Range(std::ranges::data(s),
		   std::ranges::data(s) + std::ranges::size(s));
  };

  std::basic_string<_CharT> res;
  size_t size = 0;

  Storage v1{1, 2, 3};
  res = std::format(WIDEN("{}"), makeRange(v1));
  VERIFY( res == WIDEN("[1, 2, 3]") );
  res = std::format(WIDEN("{:}"), makeRange(v1));
  VERIFY( res == WIDEN("[1, 2, 3]") );
  res = std::format(WIDEN("{:n}"), makeRange(v1));
  VERIFY( res == WIDEN("1, 2, 3") );

  res = std::format(WIDEN("{:3}"), makeRange(v1));
  VERIFY( res == WIDEN("[1, 2, 3]") );

  res = std::format(WIDEN("{:10}"), makeRange(v1));
  VERIFY( res == WIDEN("[1, 2, 3] ") );

  res = std::format(WIDEN("{:{}}"), makeRange(v1), 10);
  VERIFY( res == WIDEN("[1, 2, 3] ") );

  res = std::format(WIDEN("{1:{0}}"), 10, makeRange(v1));
  VERIFY( res == WIDEN("[1, 2, 3] ") );

  res = std::format(WIDEN("{:10n}"), makeRange(v1));
  VERIFY( res == WIDEN("1, 2, 3   ") );

  res = std::format(WIDEN("{:*<11}"), makeRange(v1));
  VERIFY( res == WIDEN("[1, 2, 3]**") );

  res = std::format(WIDEN("{:->12}"), makeRange(v1));
  VERIFY( res == WIDEN("---[1, 2, 3]") );

  res = std::format(WIDEN("{:=^13}"), makeRange(v1));
  VERIFY( res == WIDEN("==[1, 2, 3]==") );

  res = std::format(WIDEN("{:=^13n}"), makeRange(v1));
  VERIFY( res == WIDEN("===1, 2, 3===") );

  res = std::format(WIDEN("{::#x}"), makeRange(v1));
  VERIFY( res == WIDEN("[0x1, 0x2, 0x3]") );

  res = std::format(WIDEN("{:|^25n:#05x}"), makeRange(v1));
  VERIFY( res == WIDEN("|||0x001, 0x002, 0x003|||") );

  // ':' is start of the format string for element
  res = std::format(WIDEN("{::^+04}"), makeRange(v1));
  VERIFY( res == WIDEN("[ +1 ,  +2 ,  +3 ]") );

  size = std::formatted_size(WIDEN("{:}"), makeRange(v1));
  VERIFY( size == Sv(WIDEN("[1, 2, 3]")).size() );

  size = std::formatted_size(WIDEN("{:3}"), makeRange(v1));
  VERIFY( size == Sv(WIDEN("[1, 2, 3]")).size() );

  size = std::formatted_size(WIDEN("{:10}"), makeRange(v1));
  VERIFY( size == 10 );

  size = std::formatted_size(WIDEN("{:|^25n:#05x}"), makeRange(v1));
  VERIFY( size == 25 );
}

template<typename Cont>
void test_output_cont()
{
  test_output<char, Cont&, Cont>();
  test_output<wchar_t, Cont const&, Cont>();
}

template<typename View>
void test_output_view()
{
  test_output<char, View, int[3]>();
  test_output<wchar_t, View, int[3]>();
}

void
test_outputs()
{
  using namespace __gnu_test;
  test_output_cont<std::vector<int>>();
  test_output_cont<std::list<int>>();
  test_output_cont<std::array<int, 3>>();

  test_output_view<std::span<int>>();
  test_output_view<std::ranges::subrange<int*>>();
  test_output_view<test_forward_range<int>>();
  test_output_view<test_input_range<int>>();
  test_output_view<test_input_range_nocopy<int>>();

  test_output_view<std::span<const int>>();
  test_output_view<std::ranges::subrange<const int*>>();
  test_output_view<test_forward_range<const int>>();
}

void
test_nested()
{
  std::vector<std::vector<int>> v
  {
    {1, 2},
    {11, 12}
  };

  std::string res = std::format("{}", v);
  VERIFY( res == "[[1, 2], [11, 12]]" );

  res = std::format("{:+^18:n:02}", v);
  VERIFY( res == "+[01, 02, 11, 12]+" );
}

int main()
{
  test_format_string();
  test_outputs();
  test_nested();
}
