// { dg-do run { target c++23 } }
// { dg-timeout-factor 2 }

#include <format>
#include <queue>
#include <stack>
#include <testsuite_hooks.h>

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

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

template<template<typename Tp> class Adaptor>
void
test_format_string()
{
  Adaptor<int> q;
  VERIFY( !is_format_string_for("{:?}", q) );
  VERIFY( !is_format_string_for("{:P}", q) );

  // width needs to be integer type
  VERIFY( !is_format_string_for("{:{}}", q, 1.0f) );
}

struct NoFormat
{
  friend auto operator<=>(NoFormat, NoFormat) = default;
};

struct MutFormat
{
  MutFormat() = default;
  MutFormat(int p) : x(p) {}

  int x;
  friend auto operator<=>(MutFormat, MutFormat) = default;
};

template<typename CharT>
struct std::formatter<MutFormat, CharT>
  : std::formatter<int, CharT>
{
   template<typename Out>
   Out format(MutFormat& mf, basic_format_context<Out, CharT>& ctx) const
   { return std::formatter<int, CharT>::format(mf.x, ctx); }
};

template<typename T>
struct NotFormattableCont : std::vector<T>
{
  using std::vector<T>::vector;
};

template<typename T>
constexpr auto std::format_kind<NotFormattableCont<T>>
  = std::range_format::disabled;

template<typename _CharT,
	 template<typename Tp, typename Cont = std::vector<Tp>> class Adaptor>
void
test_output()
{
  const std::vector<int> v{3, 2, 1};
  std::basic_string<_CharT> res;
  Adaptor<int, std::vector<int>> q(std::from_range, v);

  res = std::format(WIDEN("{}"), q);
  VERIFY( res == WIDEN("[3, 2, 1]") );

  res = std::format(WIDEN("{}"), std::as_const(q));
  VERIFY( res == WIDEN("[3, 2, 1]") );

  res = std::format(WIDEN("{:n:#x}"), q);
  VERIFY( res == WIDEN("0x3, 0x2, 0x1") );

  res = std::format(WIDEN("{:=^23:#04x}"), q);
  VERIFY( res == WIDEN("==[0x03, 0x02, 0x01]===") );

  // Sequence output is always used
  std::queue<_CharT, std::basic_string<_CharT>> qs(
    std::from_range,
    std::basic_string_view<_CharT>(WIDEN("321")));

  res = std::format(WIDEN("{}"), qs);
  VERIFY( res == WIDEN("['3', '2', '1']") );

  res = std::format(WIDEN("{::}"), std::as_const(qs));
  VERIFY( res == WIDEN("[3, 2, 1]") );

  res = std::format(WIDEN("{:?s}"), qs);
  VERIFY( res == WIDEN(R"("321")") );

  Adaptor<int, std::deque<int>> qd(std::from_range, v);

  res = std::format(WIDEN("{}"), qd);
  VERIFY( res == WIDEN("[3, 2, 1]") );

  res = std::format(WIDEN("{}"), std::as_const(qd));
  VERIFY( res == WIDEN("[3, 2, 1]") );

  Adaptor<MutFormat> mq(std::from_range, v);

  res = std::format(WIDEN("{}"), mq);
  VERIFY( res == WIDEN("[3, 2, 1]") );

  static_assert(!std::formattable<const Adaptor<MutFormat>, _CharT>);

  static_assert(!std::formattable<Adaptor<NoFormat>, _CharT>);
  static_assert(!std::formattable<const Adaptor<NoFormat>, _CharT>);

  // Formatter check if container is formattable, not container elements.
  static_assert(!std::formattable<Adaptor<int, NotFormattableCont<int>>, _CharT>);
}

template<template<typename Tp, typename Cont = std::vector<Tp>> class Adaptor>
void
test_adaptor()
{
  test_format_string<Adaptor>();
  test_output<char, Adaptor>();
  test_output<wchar_t, Adaptor>();

  static_assert(!std::formattable<Adaptor<int>, int>);
  static_assert(!std::formattable<Adaptor<int>, char32_t>);
}

template<typename _CharT>
void
test_compare()
{
  const std::vector<int> v{3, 2, 1};
  std::basic_string<_CharT> res;
  std::priority_queue<int, std::vector<int>, std::greater<>> q(
     std::from_range, v);

  res = std::format(WIDEN("{}"), q);
  VERIFY( res == WIDEN("[1, 2, 3]") );
}

int main()
{
  test_adaptor<std::queue>();
  test_adaptor<std::priority_queue>();
  test_compare<char>();
}
