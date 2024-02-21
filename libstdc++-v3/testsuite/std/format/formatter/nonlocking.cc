// { dg-do compile { target c++23 } }

#include <format>
#include <string>

template<typename CharT>
struct MyTraits : std::char_traits<CharT>
{};

template<typename CharT>
struct MyAlloc : std::allocator<CharT>
{
  using std::allocator<CharT>::allocator;
};

template<typename CharT>
void testCharacters()
{
  static_assert(std::enable_nonlocking_formatter_optimization<
		  CharT>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  CharT*>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  const CharT*>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  CharT[5]>);

  static_assert(std::enable_nonlocking_formatter_optimization<
		  std::basic_string<CharT>>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  std::basic_string<CharT, MyTraits<CharT>>>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  std::basic_string<CharT, MyTraits<CharT>, MyAlloc<CharT>>>);

  static_assert(std::enable_nonlocking_formatter_optimization<
		  std::basic_string_view<CharT>>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  std::basic_string_view<CharT, MyTraits<CharT>>>);
}

void testAll()
{
  static_assert(std::enable_nonlocking_formatter_optimization<
		  int>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  float>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  void*>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  const void*>);
  static_assert(std::enable_nonlocking_formatter_optimization<
		  std::nullptr_t>);
   
  testCharacters<char>();
#ifdef _GLIBCXX_USE_WCHAR_T
  testCharacters<wchar_t>();
#endif // USE_WCHAR_T
}

