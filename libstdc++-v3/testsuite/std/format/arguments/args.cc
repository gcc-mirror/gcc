// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <format>
#include <testsuite_hooks.h>

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
  auto store = std::make_format_args(false, 1, '2', 3.4);
  std::format_args args = store;
  VERIFY(equals(args.get(0), false));
  VERIFY(equals(args.get(1), 1));
  VERIFY(equals(args.get(2), '2'));
  VERIFY(equals(args.get(3), 3.4));
  VERIFY(!args.get(4));

  auto cstore = std::make_format_args<std::format_context>(5L, 6ULL, 7.8f);
  std::format_args cargs = cstore;
  if constexpr (sizeof(long) == sizeof(int))
    VERIFY(equals(cargs.get(0), 5));
  else
    VERIFY(equals(cargs.get(0), 5LL));
  VERIFY(equals(cargs.get(1), 6ULL));
  VERIFY(equals(cargs.get(2), 7.8f));
  VERIFY(!cargs.get(3));

  VERIFY(equals(std::format_args(std::make_format_args(std::string("tenfour"))).get(0), std::string_view("tenfour")));

  // This needs to be on the stack so that testing pointer equality works.
  wchar_t eleven[] = L"eleven";
  // This needs to be on the stack so that the wstring_view doesn't dangle.
  std::wstring tenfour = L"tenfour";

  auto wstore = std::make_wformat_args('9', L'X', eleven, 12.13L, tenfour);
  std::wformat_args wargs = wstore;
  VERIFY(equals(wargs.get(0), static_cast<wchar_t>('9')));
  VERIFY(equals(wargs.get(1), L'X'));
  VERIFY(equals(wargs.get(2), static_cast<const wchar_t*>(eleven)));
  VERIFY(equals(wargs.get(3), 12.13L));
  VERIFY(equals(wargs.get(4), std::wstring_view(tenfour)));
  VERIFY(!wargs.get(5));

  auto another_store = std::make_format_args(nullptr, E::ByGum);
  args = another_store;
  VERIFY(equals(args.get(0), static_cast<const void*>(nullptr)));
  using handle = std::basic_format_arg<std::format_context>::handle;
  auto is_handle = []<typename T>(T) { return std::is_same_v<T, handle>; };
  VERIFY(std::visit_format_arg(is_handle, args.get(1)));
}

int main()
{
  test_empty();
  test_args();
}
