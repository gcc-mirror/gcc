// { dg-do compile { target c++17 } }

// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p2251r1.pdf

#include <string_view>

static_assert( std::is_trivially_copyable_v<std::string_view> );
static_assert( std::is_trivially_copyable_v<std::wstring_view> );

struct traits : std::char_traits<char> { };
static_assert( std::is_trivially_copyable_v<std::basic_string_view<char, traits>> );
