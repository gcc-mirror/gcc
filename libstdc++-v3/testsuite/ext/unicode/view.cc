// { dg-do run { target c++20 } }
#include <format>
#include <string_view>
#include <ranges>
#include <testsuite_hooks.h>

namespace uc = std::__unicode;
using namespace std::string_view_literals;

template<std::ranges::range View>
constexpr void
compare(View v, std::basic_string_view<std::ranges::range_value_t<View>> s)
{
  long size = s.size();
  VERIFY( std::ranges::distance(v) == size );
  VERIFY( std::ranges::equal(v,  s) );
  auto rev = std::views::reverse(v);
  VERIFY( std::ranges::distance(rev) == size );
  VERIFY( std::ranges::equal(rev,  s | std::views::reverse) );
}

constexpr void
test_utf8_to_utf8()
{
  const auto s8 = u8"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡"sv;
  uc::_Utf8_view v(s8);
  compare(v, s8);
}

constexpr void
test_utf8_to_utf16()
{
  const auto s8  = u8"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡"sv;
  const std::u16string_view s16 = u"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡";
  uc::_Utf16_view v(s8);
  compare(v, s16);
}

constexpr void
test_utf8_to_utf32()
{
  const auto s8 = u8"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡"sv;
  const auto s32 = U"£🇬🇧 €🇪🇺 æбçδé ♠♥♦♣ 🤡"sv;
  uc::_Utf32_view v(s8);
  compare(v, s32);
}

constexpr void
test_illformed_utf8()
{
  uc::_Utf32_view v("\xa3 10.99 \xee \xdd"sv);
  compare(v, U"\uFFFD 10.99 \uFFFD \uFFFD"sv);

  uc::_Utf16_view v2(" \xf8\x80\x80\x80 "sv);
  compare(v2, u" \uFFFD\uFFFD\uFFFD\uFFFD "sv);

  // Examples of U+FFFD substitution from Unicode standard.
  uc::_Utf8_view v3("\xc0\xaf\xe0\x80\xbf\xf0\x81\x82\x41"sv); // Table 3-8
  compare(v3, u8"\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\x41"sv);
  uc::_Utf8_view v4("\xed\xa0\x80\xed\xbf\xbf\xed\xaf\x41"sv); // Table 3-9
  compare(v4, u8"\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\x41"sv);
  uc::_Utf8_view v5("\xf4\x91\x92\x93\xff\x41\x80\xbf\x42"sv); // Table 3-10
  compare(v5, u8"\uFFFD\uFFFD\uFFFD\uFFFD\uFFFD\x41\uFFFD\uFFFD\x42"sv);
  uc::_Utf8_view v6("\xe1\x80\xe2\xf0\x91\x92\xf1\xbf\x41"sv); // Table 3-11
  compare(v6, u8"\uFFFD\uFFFD\uFFFD\uFFFD\x41"sv);

  uc::_Utf32_view v7("\xe1\x80"sv);
  compare(v7,  U"\uFFFD"sv);
  uc::_Utf32_view v8("\xf1\x80"sv);
  compare(v8,  U"\uFFFD"sv);
  uc::_Utf32_view v9("\xf1\x80\x80"sv);
  compare(v9,  U"\uFFFD"sv);

  uc::_Utf32_view v10("\xcf\x80\x80\x81\x82\x83 \x84\x85\x86\x87\x88 "sv);
  compare(v10,  U"\u03C0\uFFFD\uFFFD\uFFFD\uFFFD \uFFFD\uFFFD\uFFFD\uFFFD\uFFFD "sv);
  uc::_Utf16_view v11("\xcf\x80\x80\x81\x82\x83 \x84\x85\x86\x87\x88 "sv);
  compare(v11,  u"\u03C0\uFFFD\uFFFD\uFFFD\uFFFD \uFFFD\uFFFD\uFFFD\uFFFD\uFFFD "sv);
  uc::_Utf8_view v12("\xcf\x80\x80\x81\x82\x83 \x84\x85\x86\x87\x88 "sv);
  compare(v12,  u8"\u03C0\uFFFD\uFFFD\uFFFD\uFFFD \uFFFD\uFFFD\uFFFD\uFFFD\uFFFD "sv);
}

constexpr void
test_illformed_utf16()
{
  std::u16string_view s = u"\N{CLOWN FACE}";
  std::u16string_view r = u"\uFFFD";
  compare(uc::_Utf16_view(s.substr(0, 1)), r);
  compare(uc::_Utf16_view(s.substr(1, 1)), r);
  std::array s2{ s[0], s[0] };
  compare(uc::_Utf16_view(s2), u"\uFFFD\uFFFD"sv);
  std::array s3{ s[0], s[0], s[1] };
  compare(uc::_Utf16_view(s3), u"\uFFFD\N{CLOWN FACE}"sv);
  std::array s4{ s[1], s[0] };
  compare(uc::_Utf16_view(s4), u"\uFFFD\uFFFD"sv);
  std::array s5{ s[1], s[0], s[1] };
  compare(uc::_Utf16_view(s5), u"\uFFFD\N{CLOWN FACE}"sv);

  std::array<char16_t, 2> s6{ 0xDC00, 0xDC01 };
  compare(uc::_Utf16_view(s6), u"\uFFFD\uFFFD"sv);
  std::array<char16_t, 2> s7{ 0xD7FF, 0xDC00 };
  compare(uc::_Utf16_view(s7), u"\uD7FF\uFFFD"sv);
}

constexpr void
test_illformed_utf32()
{
  std::u32string_view s = U"\x110000";
  compare(uc::_Utf32_view(s), U"\uFFFD"sv);
  s = U"\xFFFFFF";
  compare(uc::_Utf32_view(s), U"\uFFFD"sv);
  s = U"\xFFFFFFF0";
  compare(uc::_Utf32_view(s), U"\uFFFD"sv);
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
  std::ranges::advance(iter, -4);
  VERIFY( *iter == U'1' );
  // Incrementing before begin has well-defined behaviour.
  iter--;
  VERIFY( *iter == U'1' );
  iter--;
  VERIFY( *iter == U'1' );

  std::string_view empty;
  uc::_Utf32_view v2(empty);
  auto iter2 = v2.begin();
  VERIFY( iter2 == v2.end() );
  VERIFY( *iter2 == U'\0' );
  iter++;
  VERIFY( iter2 == v2.end() );
  VERIFY( *iter2 == U'\0' );
  iter--;
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
