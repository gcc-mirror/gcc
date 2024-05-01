// { dg-do run { target c++20 } }
#include <format>
#include <string_view>
#include <ranges>
#include <testsuite_hooks.h>

namespace uc = std::__unicode;
using namespace std::string_view_literals;

constexpr void
test_utf8_to_utf8()
{
  const auto s8 = u8"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡"sv;
  uc::_Utf8_view v(s8);
  VERIFY( std::ranges::distance(v) == s8.size() );
  VERIFY( std::ranges::equal(v,  s8) );
}

constexpr void
test_utf8_to_utf16()
{
  const auto s8  = u8"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡"sv;
  const std::u16string_view s16 = u"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡";
  uc::_Utf16_view v(s8);
  VERIFY( std::ranges::distance(v) == s16.size() );
  VERIFY( std::ranges::equal(v,  s16) );
}

constexpr void
test_utf8_to_utf32()
{
  const auto s8 = u8"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡"sv;
  const auto s32 = U"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡"sv;
  uc::_Utf32_view v(s8);
  VERIFY( std::ranges::distance(v) == s32.size() );
  VERIFY( std::ranges::equal(v,  s32) );
}

constexpr void
test_illformed_utf8()
{
  uc::_Utf32_view v("\xa3 10.99 \xee \xdd"sv);
  VERIFY( std::ranges::equal(v, U"\uFFFD 10.99 \uFFFD \uFFFD"sv) );

  uc::_Utf16_view v2(" \xf8\x80\x80\x80 "sv);
  VERIFY( std::ranges::distance(v2) == 6 );
  VERIFY( std::ranges::equal(v2, U" \uFFFD\uFFFD\uFFFD\uFFFD "sv) );

  // Examples of U+FFFD substitution from Unicode standard.
  uc::_Utf8_view v3("\xc0\xaf\xe0\x80\xbf\xf0\x81\x82\x41"sv); // Table 3-8
  VERIFY( std::ranges::equal(v3, u8"\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\x41"sv) );
  uc::_Utf8_view v4("\xed\xa0\x80\xed\xbf\xbf\xed\xaf\x41"sv); // Table 3-9
  VERIFY( std::ranges::equal(v4, u8"\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\x41"sv) );
  uc::_Utf8_view v5("\xf4\x91\x92\x93\xff\x41\x80\xbf\x42"sv); // Table 3-10
  VERIFY( std::ranges::equal(v5, u8"\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\x41\uFFFD\uFFFD\x42"sv) );
  uc::_Utf8_view v6("\xe1\x80\xe2\xf0\x91\x92\xf1\xbf\x41"sv); // Table 3-11
  VERIFY( std::ranges::equal(v6, u8"\uFFFD\uFFFD\uFFFD\uFFFD\x41"sv) );

  uc::_Utf32_view v7("\xe1\x80"sv);
  VERIFY( std::ranges::equal(v7, U"\uFFFD"sv) );
  uc::_Utf32_view v8("\xf1\x80"sv);
  VERIFY( std::ranges::equal(v8, U"\uFFFD"sv) );
  uc::_Utf32_view v9("\xf1\x80\x80"sv);
  VERIFY( std::ranges::equal(v9, U"\uFFFD"sv) );
}

constexpr void
test_illformed_utf16()
{
  std::u16string_view s = u"\N{CLOWN FACE}";
  std::u16string_view r = u"\uFFFD";
  VERIFY( std::ranges::equal(uc::_Utf16_view(s.substr(0, 1)), r) );
  VERIFY( std::ranges::equal(uc::_Utf16_view(s.substr(1, 1)), r) );
  std::array s2{ s[0], s[0] };
  VERIFY( std::ranges::equal(uc::_Utf16_view(s2), u"\uFFFD\uFFFD"sv) );
  std::array s3{ s[0], s[0], s[1] };
  VERIFY( std::ranges::equal(uc::_Utf16_view(s3), u"\uFFFD\N{CLOWN FACE}"sv) );
  std::array s4{ s[1], s[0] };
  VERIFY( std::ranges::equal(uc::_Utf16_view(s4), u"\uFFFD\uFFFD"sv) );
  std::array s5{ s[1], s[0], s[1] };
  VERIFY( std::ranges::equal(uc::_Utf16_view(s5), u"\uFFFD\N{CLOWN FACE}"sv) );
}

constexpr void
test_illformed_utf32()
{
  std::u32string_view s = U"\x110000";
  VERIFY( std::ranges::equal(uc::_Utf32_view(s), U"\uFFFD"sv) );
  s = U"\xFFFFFF";
  VERIFY( std::ranges::equal(uc::_Utf32_view(s), U"\uFFFD"sv) );
  s = U"\xFFFFFFF0";
  VERIFY( std::ranges::equal(uc::_Utf32_view(s), U"\uFFFD"sv) );
}

constexpr void
test_past_the_end()
{
  const auto s8 = u8"1234"sv;
  uc::_Utf32_view v(s8);
  auto iter = v.begin();
  std::advance(iter, 4);
  VERIFY( iter == v.end() );
  // Incrementing past the end has well-defined behaviour.
  ++iter;
  VERIFY( iter == v.end() );
  VERIFY( *iter == U'4' ); // Still dereferenceable.
  ++iter;
  VERIFY( iter == v.end() );
  VERIFY( *iter == U'4' );
  iter++;
  VERIFY( iter == v.end() );
  VERIFY( *iter == U'4' );

  std::string_view empty;
  uc::_Utf32_view v2(empty);
  auto iter2 = v2.begin();
  VERIFY( iter2 == v2.end() );
  VERIFY( *iter2 == U'\0' );
  iter++;
  VERIFY( iter2 == v2.end() );
  VERIFY( *iter2 == U'\0' );
}

int main()
{
  auto run_tests = []{
    test_utf8_to_utf8();
    test_utf8_to_utf16();
    test_utf8_to_utf32();
    test_illformed_utf8();
    test_illformed_utf16();
    test_illformed_utf32();
    test_past_the_end();
    return true;
  };

  VERIFY( run_tests() );
  static_assert( run_tests() );
}
