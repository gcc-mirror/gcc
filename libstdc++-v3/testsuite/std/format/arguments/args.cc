// { dg-do run { target c++20 } }

// { dg-warning "deprecated" "std::visit_format_arg" { target c++26 } 0 }

#include <format>
#include <testsuite_hooks.h>

// LWG 4106. basic_format_args should not be default-constructible
static_assert( ! std::is_default_constructible_v<std::format_args> );
static_assert( ! std::is_default_constructible_v<std::wformat_args> );

template<typename Ctx, typename T>
bool equals(std::basic_format_arg<Ctx> fmt_arg, T expected) {
  return std::visit_format_arg([=](auto arg_val) {
    if constexpr (std::is_same_v<decltype(arg_val), T>)
      return arg_val == expected;
    else
      return false;
  }, fmt_arg);
}

void
test_empty()
{
  std::format_args args = std::make_format_args();
  VERIFY(!args.get(0));
  VERIFY(!args.get(1));
  VERIFY(!args.get((std::size_t)-1));
  VERIFY(equals(args.get(0), std::monostate{}));

  std::format_args cargs =  std::make_format_args<std::format_context>();
  VERIFY(!cargs.get(0));
  VERIFY(equals(cargs.get(0), std::monostate{}));

  std::wformat_args wargs = std::make_wformat_args();
  VERIFY(!wargs.get(0));
  VERIFY(equals(wargs.get(0), std::monostate{}));
}

enum E { ByGum };

template<>
struct std::formatter<E> : std::formatter<int>
{
  using std::formatter<int>::parse;

  std::format_context::iterator
  format(E e, std::format_context& fc) const
  { return std::formatter<int>::format((int)e, fc); }
};

void
test_args()
{
  bool b = false;
  int i = 1;
  char c = '2';
  double d = 3.4;

  // We need an lvalue of type format-arg-store<Context, Args...> for the
  // lvalue args to point to, otherwise it will have a dangling pointer.
  // This is not how you're supposed to use format args, just for testing.
  auto store = std::make_format_args(b, i, c, d);
  std::format_args args = store;
  VERIFY(equals(args.get(0), false));
  VERIFY(equals(args.get(1), 1));
  VERIFY(equals(args.get(2), '2'));
  VERIFY(equals(args.get(3), 3.4));
  VERIFY(!args.get(4));

  long l = 5L;
  unsigned long long ull = 6ULL;
  float f = 7.8f;

  auto cstore = std::make_format_args<std::format_context>(l, ull, f);
  std::format_args cargs = cstore;
  if constexpr (sizeof(long) == sizeof(int))
    VERIFY(equals(cargs.get(0), 5));
  else
    VERIFY(equals(cargs.get(0), 5LL));
  VERIFY(equals(cargs.get(1), 6ULL));
  VERIFY(equals(cargs.get(2), 7.8f));
  VERIFY(!cargs.get(3));

  std::string s = "tenfour";
  VERIFY(equals(std::format_args(std::make_format_args(s)).get(0), std::string_view("tenfour")));

  char nine = '9';
  wchar_t ten = L'X';
  // This needs to be on the stack so that testing pointer equality works.
  wchar_t eleven[] = L"eleven";
  long double twelve13 = 12.13L;
  std::wstring tenfour = L"tenfour";

  auto wstore = std::make_wformat_args(nine, ten, eleven, twelve13, tenfour);
  std::wformat_args wargs = wstore;
  VERIFY(equals(wargs.get(0), static_cast<wchar_t>('9')));
  VERIFY(equals(wargs.get(1), L'X'));
  VERIFY(equals(wargs.get(2), static_cast<const wchar_t*>(eleven)));
  VERIFY(equals(wargs.get(3), 12.13L));
  VERIFY(equals(wargs.get(4), std::wstring_view(tenfour)));
  VERIFY(!wargs.get(5));

  std::nullptr_t null;
  E eebygum = E::ByGum;
  auto another_store = std::make_format_args(null, eebygum);
  args = another_store;
  VERIFY(equals(args.get(0), static_cast<const void*>(nullptr)));
  using handle = std::basic_format_arg<std::format_context>::handle;
  auto is_handle = []<typename T>(T) { return std::is_same_v<T, handle>; };
  VERIFY(std::visit_format_arg(is_handle, args.get(1)));
}

void
test_member_visit()
{
#if __cpp_lib_format >= 202306L // C++26 adds std::basic_format_arg::visit
  int one = 1;
  float two = 2.0;
  std::string_view three = "three";
  auto store = std::make_format_args(one, two, three); // See comment above.
  std::format_args args = store;
  auto a1 = args.get(0).visit([=](auto a) {
    if constexpr (std::is_same_v<decltype(a), int>)
      return a == one;
    return false;
  });
  VERIFY( a1 );
  auto a2 = args.get(1).visit([=](auto a) {
    if constexpr (std::is_same_v<decltype(a), float>)
      return a == two;
    return false;
  });
  VERIFY( a2 );
  auto a3 = args.get(2).visit([=](auto a) {
    if constexpr (std::is_same_v<decltype(a), std::string_view>)
      return a == three;
    return false;
  });
  VERIFY( a3 );

  auto a4 = args.get(0).visit<std::string_view>([](auto) { return "str"; });
  static_assert( std::is_same_v<decltype(a4), std::string_view> );
  VERIFY( a4 == "str" );
  args.get(0).visit<void>([](auto){});
  using V = decltype(args.get(0).visit<void>([](auto){}));
  static_assert( std::is_same_v<V, void> );
#endif
}

template<typename T>
void test_visited_as_handle()
{
  T v{};
  auto store = std::make_format_args(v);
  std::format_args args = store;

  constexpr auto is_handle = [](auto arg) {
    return std::is_same_v<decltype(arg), decltype(args.get(0))::handle>;
  };
  VERIFY( std::visit_format_arg(is_handle, args.get(0)) );
#if __cpp_lib_format >= 202306L // C++26 adds std::basic_format_arg::visit
  VERIFY( args.get(0).visit(is_handle) );
#endif
}

template<typename E, typename S>
void test_visited_as()
{
  auto v = static_cast<S>(1.0);
  auto store = std::make_format_args(v);
  std::format_args args = store;

  auto is_expected_val = [v](auto arg) {
    if constexpr (std::is_same_v<decltype(arg), E>)
      return arg == static_cast<E>(v);
    return false;
  };
  VERIFY( std::visit_format_arg(is_expected_val, args.get(0)) );
#if __cpp_lib_format >= 202306L // C++26 adds std::basic_format_arg::visit
  VERIFY( args.get(0).visit(is_expected_val) );
#endif
}

template<typename T>
concept can_format = std::is_default_constructible_v<std::formatter<T, char>>;

int main()
{
  test_empty();
  test_args();
  test_member_visit();

#ifdef __SIZEOF_INT128__
  test_visited_as_handle<__int128>();
  test_visited_as_handle<unsigned __int128>();
#endif
// TODO: This should be visited as handle.
#ifdef __STDCPP_FLOAT16_T__
  if constexpr (can_format<_Float16>)
    test_visited_as<float, _Float16>();
#endif
#ifdef __STDCPP_BFLOAT16_T__
  if constexpr (can_format<__gnu_cxx::__bfloat16_t>)
    test_visited_as<float, __gnu_cxx::__bfloat16_t>();
#endif
#ifdef __FLT32_DIG__
  if constexpr (can_format<_Float32>)
    test_visited_as<float, _Float32>();
#endif
#ifdef __FLT64_DIG__
  if constexpr (can_format<_Float64>)
    test_visited_as<double, _Float64>();
#endif
#ifdef __FLT128_DIG__
  if constexpr (can_format<_Float128>)
# ifdef _GLIBCXX_LDOUBLE_IS_IEEE_BINARY128
    test_visited_as<long double, _Float128>();
# else
    test_visited_as_handle<_Float128>();
# endif
#endif
#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
  if constexpr (!std::is_same_v<__ieee128, long double>)
    test_visited_as_handle<__ieee128>();
  if constexpr (!std::is_same_v<__ibm128, long double>)
    test_visited_as_handle<__ibm128>();
#endif
}
