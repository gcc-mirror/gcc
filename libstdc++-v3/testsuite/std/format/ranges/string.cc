// { dg-do run { target c++23 } }

#include <format>
#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <vector>

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
  // only CharT value types are supported
  VERIFY( !is_range_formatter_spec_for(L"s", std::vector<char>()) );
  VERIFY( !is_format_string_for(L"{:s}", std::vector<char>()) );
  VERIFY( !is_range_formatter_spec_for(L"s", std::vector<char>()) );
  VERIFY( !is_format_string_for(L"{:s}", std::vector<char>()) );
  VERIFY( !is_range_formatter_spec_for("s", std::vector<int>()) );
  VERIFY( !is_format_string_for("{:s}", std::vector<int>()) );

  // invalid format stringss
  VERIFY( !is_range_formatter_spec_for("?", std::vector<char>()) );
  VERIFY( !is_format_string_for("{:?}", std::vector<char>()) );
  VERIFY( !is_range_formatter_spec_for("ns", std::vector<char>()) );
  VERIFY( !is_format_string_for("{:ns}", std::vector<char>()) );
  VERIFY( !is_range_formatter_spec_for("s:", std::vector<char>()) );
  VERIFY( !is_format_string_for("{:s:}", std::vector<char>()) );

  // precision is not supported, even for s
  VERIFY( !is_range_formatter_spec_for(".10s", std::vector<char>()) );
  VERIFY( !is_format_string_for("{:.10s}", std::vector<char>()) );
  VERIFY( !is_format_string_for("{:.{}s}", std::vector<char>(), 10) );

  // width needs to be integer type
  VERIFY( !is_format_string_for("{:{}s}", std::vector<char>(), 1.0f) );
}

template<typename Range>
void test_output()
{
  using _CharT = std::ranges::range_value_t<Range>;
  auto makeRange = [](std::basic_string<_CharT>& s) {
    return Range(s.data(), s.data() + s.size());
  };
  std::basic_string<_CharT> res;
  size_t size = 0;

  std::basic_string<_CharT> s1 = WIDEN("abcd");
  res = std::format(WIDEN("{}"), makeRange(s1));
  VERIFY( res == WIDEN("['a', 'b', 'c', 'd']") );

  res = std::format(WIDEN("{::}"), makeRange(s1));
  VERIFY( res == WIDEN("[a, b, c, d]") );

  res = std::format(WIDEN("{:s}"), makeRange(s1));
  VERIFY( res == WIDEN("abcd") );

  res = std::format(WIDEN("{:?s}"), makeRange(s1));
  VERIFY( res == WIDEN(R"("abcd")") );

  res = std::format(WIDEN("{:3s}"), makeRange(s1));
  VERIFY( res == WIDEN("abcd") );

  res = std::format(WIDEN("{:7s}"), makeRange(s1));
  VERIFY( res == WIDEN("abcd   ") );

  res = std::format(WIDEN("{:{}s}"), makeRange(s1), 7);
  VERIFY( res == WIDEN("abcd   ") );

  res = std::format(WIDEN("{1:{0}s}"), 7, makeRange(s1));
  VERIFY( res == WIDEN("abcd   ") );

  res = std::format(WIDEN("{:*>6s}"), makeRange(s1));
  VERIFY( res == WIDEN("**abcd") );

  res = std::format(WIDEN("{:-<5s}"), makeRange(s1));
  VERIFY( res == WIDEN("abcd-") );

  res = std::format(WIDEN("{:=^8s}"), makeRange(s1));
  VERIFY( res == WIDEN("==abcd==") );

  std::basic_string<_CharT> s2(512, static_cast<_CharT>('a'));
  res = std::format(WIDEN("{:=^8s}"), makeRange(s2));
  VERIFY( res == s2 );

  size = std::formatted_size(WIDEN("{:s}"), makeRange(s1));
  VERIFY( size == 4 );

  size = std::formatted_size(WIDEN("{:3s}"), makeRange(s1));
  VERIFY( size == 4 );

  size = std::formatted_size(WIDEN("{:7s}"), makeRange(s1));
  VERIFY( size == 7 );

  size = std::formatted_size(WIDEN("{:s}"), makeRange(s2));
  VERIFY( size == 512 );
}

template<typename CharT>
struct cstr_view
{
  cstr_view() = default;
  explicit cstr_view(CharT* f, CharT* l)
  : ptr(f)
  { VERIFY(!*l); }

  struct sentinel
  {
    friend constexpr
    bool operator==(CharT const* ptr, sentinel) noexcept
    { return !*ptr; }
  };

  constexpr
  CharT* begin() const noexcept
  { return ptr; };
  static constexpr
  sentinel end() noexcept
  { return {}; }

private:
  CharT* ptr = "";
};

template<typename CharT>
void
test_outputs()
{
  using namespace __gnu_test;
  test_output<std::vector<CharT>>();
  test_output<std::span<CharT>>();
  test_output<cstr_view<CharT>>();

  test_output<test_forward_range<CharT>>();
  test_output<test_forward_sized_range<CharT>>();

  test_output<test_input_range<CharT>>();
  test_output<test_input_sized_range<CharT>>();

  test_output<test_range_nocopy<CharT, input_iterator_wrapper_nocopy>>();
  test_output<test_sized_range<CharT, input_iterator_wrapper_nocopy>>();

  test_output<std::span<const CharT>>();
  test_output<cstr_view<const CharT>>();
  test_output<test_forward_range<const CharT>>();

  static_assert(!std::formattable<std::span<volatile CharT>, CharT>);
  static_assert(!std::formattable<std::span<const volatile CharT>, CharT>);
}

void
test_nested()
{
  std::string_view s1 = "str1";
  std::string_view s2 = "str2";

  std::vector<std::string> vs;
  vs.emplace_back(s1);
  vs.emplace_back(s2);

  VERIFY( std::format("{}", vs) == R"(["str1", "str2"])" );
  VERIFY( std::format("{:}", vs) == R"(["str1", "str2"])" );
  VERIFY( std::format("{::?}", vs) == R"(["str1", "str2"])" );
  VERIFY( std::format("{::}", vs) == R"([str1, str2])" );

  std::vector<std::vector<char>> vv;
  vv.emplace_back(s1.begin(), s1.end());
  vv.emplace_back(s2.begin(), s2.end());
  std::string_view escaped = R"([['s', 't', 'r', '1'], ['s', 't', 'r', '2']])";

  VERIFY( std::format("{}", vv) == escaped );
  VERIFY( std::format("{:}", vv) == escaped );
  VERIFY( std::format("{::}", vv) == escaped );
  VERIFY( std::format("{:::?}", vv) == escaped );
  VERIFY( std::format("{:::}", vv) == R"([[s, t, r, 1], [s, t, r, 2]])" );
  VERIFY( std::format("{::s}", vv) == R"([str1, str2])" );
  VERIFY( std::format("{::?s}", vv) == R"(["str1", "str2"])" );
}

int main()
{
  test_format_string();
  test_outputs<char>();
  test_outputs<wchar_t>();
  test_nested();
}
