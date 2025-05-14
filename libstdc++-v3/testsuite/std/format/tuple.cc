// { dg-do run { target c++23 } }
// { dg-options "-fexec-charset=UTF-8" }
// { dg-timeout-factor 2 }

#include <format>
#include <string>
#include <testsuite_hooks.h>
#include <tuple>
#include <utility>

struct NotFormattable
{};

static_assert( !std::formattable<std::pair<int, NotFormattable>, char> );
static_assert( !std::formattable<std::tuple<int, NotFormattable, int>, wchar_t> );

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

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

void
test_format_string()
{
  // invalid format stringss
  VERIFY( !is_format_string_for("{:p}", std::tuple<>()) );
  VERIFY( !is_format_string_for("{:nm}", std::tuple<>()) );

  // 'm' is only valid for 2 elemenst
  VERIFY( !is_format_string_for("{:m}", std::tuple<>()) );
  VERIFY( !is_format_string_for("{:m}", std::tuple<int, int, int>()) );

  // element specifier is not supported
  VERIFY( !is_format_string_for("{::}", std::tuple<>()) );

  // precision is not supported
  VERIFY( !is_format_string_for("{:.10}", std::tuple<>()) );

  // width needs to be integer type
  VERIFY( !is_format_string_for("{:{}}", std::tuple<>(), 1.0f) );
}

template<typename _CharT>
void test_multi()
{
  using Sv = std::basic_string_view<_CharT>;
  using Str = std::basic_string<_CharT>;

  std::basic_string<_CharT> res;
  std::size_t size = 0;
  std::tuple<int, Str, float> t1(1, WIDEN("test"), 2.1);

  res = std::format(WIDEN("{}"), t1);
  VERIFY( res == WIDEN(R"((1, "test", 2.1))") );
  res = std::format(WIDEN("{:}"), t1);
  VERIFY( res == WIDEN(R"((1, "test", 2.1))") );
  res = std::format(WIDEN("{:n}"), t1);
  VERIFY( res == WIDEN(R"(1, "test", 2.1)") );

  res = std::format(WIDEN("{:3}"), t1);
  VERIFY( res == WIDEN(R"((1, "test", 2.1))") );

  res = std::format(WIDEN("{:20}"), t1);
  VERIFY( res == WIDEN(R"((1, "test", 2.1)    )") );

  res = std::format(WIDEN("{:{}}"), t1, 20);
  VERIFY( res == WIDEN(R"((1, "test", 2.1)    )") );

  res = std::format(WIDEN("{1:{0}}"), 20, t1);
  VERIFY( res == WIDEN(R"((1, "test", 2.1)    )") );

  res = std::format(WIDEN("{:^>17}"), t1);
  VERIFY( res == WIDEN(R"(^(1, "test", 2.1))") );

  res = std::format(WIDEN("{:$<18}"), t1);
  VERIFY( res == WIDEN(R"((1, "test", 2.1)$$)") );

  res = std::format(WIDEN("{:+^19}"), t1);
  VERIFY( res == WIDEN(R"(+(1, "test", 2.1)++)") );

  res = std::format(WIDEN("{:|^19n}"), t1);
  VERIFY( res == WIDEN(R"(||1, "test", 2.1|||)") );

  size = std::formatted_size(WIDEN("{}"), t1);
  VERIFY( size == Sv(WIDEN(R"((1, "test", 2.1))")).size() );

  size = std::formatted_size(WIDEN("{:3}"), t1);
  VERIFY( size == Sv(WIDEN(R"((1, "test", 2.1))")).size() );

  size = std::formatted_size(WIDEN("{:20}"), t1);
  VERIFY( size == 20 );

  std::tuple<int&, Str&, float&> t2 = t1;
  res = std::format(WIDEN("{}"), t2);
  VERIFY( res == WIDEN(R"((1, "test", 2.1))") );

  std::tuple<int, int, int, int> t3(1, 2, 3, 4);
  res = std::format(WIDEN("{}"), t3);
  VERIFY( res == WIDEN(R"((1, 2, 3, 4))") );

}

template<typename _CharT, typename Tuple>
void test_empty()
{
  std::basic_string<_CharT> res;

  Tuple e1;
  res = std::format(WIDEN("{}"), e1);
  VERIFY( res == WIDEN(R"(())") );

  res = std::format(WIDEN("{:}"), e1);
  VERIFY( res == WIDEN(R"(())") );

  res = std::format(WIDEN("{:n}"), e1);
  VERIFY( res == WIDEN(R"()") );

  res = std::format(WIDEN("{:^>6}"), e1);
  VERIFY( res == WIDEN(R"(^^^^())") );
}

template<typename _CharT, typename Pair>
void test_pair()
{
  using Ft = std::remove_cvref_t<std::tuple_element_t<0, Pair>>;
  using St = std::remove_cvref_t<std::tuple_element_t<1, Pair>>;

  std::basic_string<_CharT> res;

  Ft f1 = 1;
  St s1 = WIDEN("abc");
  Pair p1(f1, s1);

  res = std::format(WIDEN("{}"), p1);
  VERIFY( res == WIDEN(R"((1, "abc"))") );

  res = std::format(WIDEN("{:}"), p1);
  VERIFY( res == WIDEN(R"((1, "abc"))") );

  res = std::format(WIDEN("{:m}"), p1);
  VERIFY( res == WIDEN(R"(1: "abc")") );

  res = std::format(WIDEN("{:|^12m}"), p1);
  VERIFY( res == WIDEN(R"(||1: "abc"||)") );
}

template<typename CharT, template<typename, typename> class PairT>
void test_pair_e()
{
  test_pair<CharT, PairT<int, std::basic_string<CharT>>>();
  test_pair<CharT, PairT<int, const CharT*>>();
  test_pair<CharT, PairT<const int, std::basic_string<CharT>>>();
  test_pair<CharT, PairT<int&, std::basic_string<CharT>&>>();
  test_pair<CharT, PairT<const int&, const std::basic_string<CharT>&>>();
}

template<typename Pair>
struct MyPair : Pair
{
  using Pair::Pair;
};

template<typename Pair, typename CharT>
struct std::formatter<MyPair<Pair>, CharT>
{
  constexpr formatter() noexcept
  {
    using _CharT = CharT;
    _formatter.set_brackets(WIDEN("<"), WIDEN(">"));
    _formatter.set_separator(WIDEN("; "));
  }

  constexpr std::basic_format_parse_context<CharT>::iterator
  parse(std::basic_format_parse_context<CharT>& pc)
  { return _formatter.parse(pc);  }

  template<typename Out>
  typename std::basic_format_context<Out, CharT>::iterator
  format(const MyPair<Pair>& mp,
	 std::basic_format_context<Out, CharT>& fc) const
  { return _formatter.format(mp, fc); }

private:
  std::formatter<Pair, CharT> _formatter;
};

template<typename _CharT, template<typename, typename> class PairT>
void test_custom()
{
  std::basic_string<_CharT> res;
  MyPair<PairT<int, const _CharT*>> c1(1, WIDEN("abc"));

  res = std::format(WIDEN("{}"), c1);
  VERIFY( res == WIDEN(R"(<1; "abc">)") );

  res = std::format(WIDEN("{:}"), c1);
  VERIFY( res == WIDEN(R"(<1; "abc">)") );

  res = std::format(WIDEN("{:n}"), c1);
  VERIFY( res == WIDEN(R"(1; "abc")") );

  res = std::format(WIDEN("{:m}"), c1);
  VERIFY( res == WIDEN(R"(1: "abc")") );

  res = std::format(WIDEN("{:|^14}"), c1);
  VERIFY( res == WIDEN(R"(||<1; "abc">||)") );
}

template<typename CharT>
void test_outputs()
{
  test_multi<CharT>();
  test_empty<CharT, std::tuple<>>();
  test_pair_e<CharT, std::pair>();
  test_pair_e<CharT, std::tuple>();
  test_custom<CharT, std::pair>();
  test_custom<CharT, std::tuple>();
}

void test_nested()
{
  std::string res;
  std::tuple<std::tuple<>, std::pair<int, std::string>> tt{{}, {1, "abc"}};

  res = std::format("{}", tt);
  VERIFY( res == R"(((), (1, "abc")))" );
  res = std::format("{:n}", tt);
  VERIFY( res == R"((), (1, "abc"))" );
  res = std::format("{:m}", tt);
  VERIFY( res == R"((): (1, "abc"))" );
}

bool strip_quote(std::string_view& v)
{
  if (!v.starts_with('"'))
    return false;
  v.remove_prefix(1);
  return true;
}

bool strip_prefix(std::string_view& v, std::string_view expected, bool quoted = false)
{
  if (quoted && !strip_quote(v))
    return false;
  if (!v.starts_with(expected))
    return false;
  v.remove_prefix(expected.size());
  if (quoted && !strip_quote(v))
    return false;
  return true;
}

bool strip_parens(std::string_view& v)
{
  if (!v.starts_with('(') || !v.ends_with(')'))
    return false;
  v.remove_prefix(1);
  v.remove_suffix(1);
  return true;
}

bool strip_prefix(std::string_view& v, size_t n, char c)
{
  size_t pos = v.find_first_not_of(c);
  if (pos == std::string_view::npos)
    pos = v.size();
  if (pos != n)
    return false;
  v.remove_prefix(n);
  return true;
}

void test_padding()
{
  std::string res;
  std::string_view resv;

  // width is 3, size is 15
  std::string in = "o\u0302\u0323i\u0302\u0323u\u0302\u0323";
  in += in; // width is 6, size is 30
  in += in; // width is 12, size is 60
  in += in; // width is 24, size is 120
  in += in; // width is 48, size is 240
  // width is 192, size is 960
  auto const ts = std::make_tuple(in, in, in, in);

  auto const check_elems = [=](std::string_view& v)
  {
    VERIFY( strip_prefix(v, in, true) );
    VERIFY( strip_prefix(v, ", ", false) );
    VERIFY( strip_prefix(v, in, true) );
    VERIFY( strip_prefix(v, ", ", false) );
    VERIFY( strip_prefix(v, in, true) );
    VERIFY( strip_prefix(v, ", ", false) );
    VERIFY( strip_prefix(v, in, true) );
    return v.empty();
  };

  resv = res = std::format("{}", ts);
  VERIFY( strip_parens(resv) );
  VERIFY( check_elems(resv) );

  resv = res = std::format("{:n}", ts);
  VERIFY( check_elems(resv) );

  resv = res = std::format("{:*>10}", ts);
  VERIFY( strip_parens(resv) );
  VERIFY( check_elems(resv) );

  resv = res = std::format("{:*>10n}", ts);
  VERIFY( check_elems(resv) );

  resv = res = std::format("{:*>240}", ts);
  VERIFY( strip_prefix(resv, 32, '*') );
  VERIFY( strip_parens(resv) );
  VERIFY( check_elems(resv) );

  resv = res = std::format("{:*>240n}", ts);
  VERIFY( strip_prefix(resv, 34, '*') );
  VERIFY( check_elems(resv) );
}

int main()
{
  test_format_string();
  test_outputs<char>();
  test_outputs<wchar_t>();
  test_nested();
  test_padding();
}
