// { dg-do run { target c++23 } }

#include <flat_map>
#include <format>
#include <list>
#include <map>
#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <vector>

struct NotFormattable
{
  friend auto operator<=>(NotFormattable, NotFormattable) = default;
};

static_assert( !std::formattable<std::map<int, NotFormattable>, char> );
static_assert( !std::formattable<std::map<NotFormattable, int>, wchar_t> );

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

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

void
test_format_string()
{
  // only pair<T, U> amd tuple<T, U> value types are supported
  VERIFY( !is_range_formatter_spec_for("m", std::vector<int>()) );
  VERIFY( !is_format_string_for("{:m}", std::vector<int>()) );
  VERIFY( !is_range_formatter_spec_for("m", std::vector<std::tuple<int, int, int>>()) );
  VERIFY( !is_format_string_for("{:m}", std::vector<std::tuple<int, int, int>>()) );

  // invalid format stringss
  VERIFY( !is_range_formatter_spec_for("?m", std::vector<std::pair<int, int>>()) );
  VERIFY( !is_format_string_for("{:?m}", std::vector<std::pair<int, int>>()) );
  VERIFY( !is_range_formatter_spec_for("m:", std::vector<std::pair<int, int>>()) );
  VERIFY( !is_format_string_for("{:m:}", std::vector<std::pair<int, int>>()) );

  // precision is not supported
  VERIFY( !is_range_formatter_spec_for(".10m", std::vector<std::pair<int, int>>()) );
  VERIFY( !is_format_string_for("{:.10m}", std::vector<std::pair<int, int>>()) );
  VERIFY( !is_format_string_for("{:.{}m}", std::vector<std::pair<int, int>>(), 10) );

  // width needs to be integer type
  VERIFY( !is_format_string_for("{:{}m}", std::vector<std::pair<int, int>>(), 1.0f) );
}

template<typename _CharT, typename Range>
void test_output(bool mapIsDefault)
{
  using Sv = std::basic_string_view<_CharT>;
  using Pt = std::ranges::range_value_t<Range>;
  using Ft = std::remove_cvref_t<std::tuple_element_t<0, Pt>>;
  using St = std::remove_cvref_t<std::tuple_element_t<1, Pt>>;
  auto makeRange = [](std::span<Pt> s) {
    return Range(s.data(), s.data() + s.size());
  };

  std::basic_string<_CharT> res;
  size_t size = 0;

  Ft f1[]{1, 2, 3};
  St s1[]{11, 22, 33};
  Pt v1[]{{f1[0], s1[0]}, {f1[1], s1[1]}, {f1[2], s1[2]}};

  res = std::format(WIDEN("{}"), makeRange(v1));
  if (mapIsDefault)
    VERIFY( res == WIDEN("{1: 11, 2: 22, 3: 33}") );
  else
    VERIFY( res == WIDEN("[(1, 11), (2, 22), (3, 33)]") );

  res = std::format(WIDEN("{:m}"), makeRange(v1));
  VERIFY( res == WIDEN("{1: 11, 2: 22, 3: 33}") );
  res = std::format(WIDEN("{:nm}"), makeRange(v1));
  VERIFY( res == WIDEN("1: 11, 2: 22, 3: 33") );

  res = std::format(WIDEN("{:3m}"), makeRange(v1));
  VERIFY( res == WIDEN("{1: 11, 2: 22, 3: 33}") );

  res = std::format(WIDEN("{:25m}"), makeRange(v1));
  VERIFY( res == WIDEN("{1: 11, 2: 22, 3: 33}    ") );

  res = std::format(WIDEN("{:{}m}"), makeRange(v1), 25);
  VERIFY( res == WIDEN("{1: 11, 2: 22, 3: 33}    ") );

  res = std::format(WIDEN("{1:{0}m}"), 25, makeRange(v1));
  VERIFY( res == WIDEN("{1: 11, 2: 22, 3: 33}    ") );

  res = std::format(WIDEN("{:25nm}"), makeRange(v1));
  VERIFY( res == WIDEN("1: 11, 2: 22, 3: 33      ") );

  res = std::format(WIDEN("{:*<23m}"), makeRange(v1));
  VERIFY( res == WIDEN("{1: 11, 2: 22, 3: 33}**") );

  res = std::format(WIDEN("{:->24m}"), makeRange(v1));
  VERIFY( res == WIDEN("---{1: 11, 2: 22, 3: 33}") );

  res = std::format(WIDEN("{:=^25m}"), makeRange(v1));
  VERIFY( res == WIDEN("=={1: 11, 2: 22, 3: 33}==") );

  res = std::format(WIDEN("{:=^25nm}"), makeRange(v1));
  VERIFY( res == WIDEN("===1: 11, 2: 22, 3: 33===") );

  size = std::formatted_size(WIDEN("{:m}"), makeRange(v1));
  VERIFY( size == Sv(WIDEN("{1: 11, 2: 22, 3: 33}")).size() );

  size = std::formatted_size(WIDEN("{:3m}"), makeRange(v1));
  VERIFY( size == Sv(WIDEN("{1: 11, 2: 22, 3: 33}")).size() );

  size = std::formatted_size(WIDEN("{:25m}"), makeRange(v1));
  VERIFY( size == 25 );
}

template<class Range>
void test_output_c(bool mapIsDefault = false)
{
  test_output<char, Range>(mapIsDefault);
  test_output<wchar_t, Range>(mapIsDefault);
}

template<template<typename> class RangeT>
void test_output_pc()
{
  test_output_c<RangeT<std::pair<int, int>>>();
  test_output_c<RangeT<std::pair<const int, int>>>();
  test_output_c<RangeT<std::tuple<const int&, int&>>>();
}

void
test_outputs()
{
  using namespace __gnu_test;
  test_output_c<std::map<int, int>>(true);
  test_output_c<std::flat_map<int, int>>(true);

  test_output_pc<std::vector>();
  test_output_pc<std::list>();
  test_output_pc<std::span>();

  test_output_pc<test_forward_range>();
  test_output_pc<test_input_range>();
  test_output_pc<test_input_range_nocopy>();
}

void
test_nested()
{
  std::vector<std::map<int, std::string>> vm{
    {{1, "one"}, {2, "two"}},
    {{1, "jeden"}, {2, "dwa"}},
  };
  std::string res;

  res = std::format("{}", vm);
  VERIFY( res == R"([{1: "one", 2: "two"}, {1: "jeden", 2: "dwa"}])" );
  res = std::format("{:n:n}", vm);
  VERIFY( res == R"(1: "one", 2: "two", 1: "jeden", 2: "dwa")" );

  std::map<std::string, std::vector<std::string>> mv{
    {"english", {"zero", "one", "two"}},
    {"polish", {"zero", "jeden", "dwa"}},
  };
  res = std::format("{}", mv);
  VERIFY( res == R"({"english": ["zero", "one", "two"], "polish": ["zero", "jeden", "dwa"]})" );
}

int main()
{
  test_format_string();
  test_outputs();
  test_nested();
}
