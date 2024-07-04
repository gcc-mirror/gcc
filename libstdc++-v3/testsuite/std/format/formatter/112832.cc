// { dg-do compile { target c++20 } }

#include <format>

template<typename T,
	 typename C = std::remove_cvref_t<decltype(std::declval<T&>()[0])>>
constexpr bool
test_pr112832()
{
  std::formatter<T, C> f;
  if constexpr (requires{ f.set_debug_format(); })
    f.set_debug_format();
  return true;
}

int main()
{
  static_assert(test_pr112832<std::string_view>());
  static_assert(test_pr112832<char*>());
  static_assert(test_pr112832<const char*>());
  static_assert(test_pr112832<char[1]>());
#ifdef _GLIBCXX_USE_WCHAR_T
  static_assert(test_pr112832<std::wstring_view>());
  static_assert(test_pr112832<wchar_t*>());
  static_assert(test_pr112832<const wchar_t*>());
  static_assert(test_pr112832<wchar_t[1]>());
  static_assert(test_pr112832<char, wchar_t>());
#endif
}
