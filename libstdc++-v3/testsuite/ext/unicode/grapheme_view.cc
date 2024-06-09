// { dg-do compile { target c++20 } }

#include <format> // includes <bits/unicode.h>
#include <string_view>
#include <ranges>
#include <testsuite_hooks.h>

namespace uc = std::__unicode;
using namespace std::string_view_literals;

constexpr void
test_breaks()
{
  VERIFY(uc::__field_width(u8"\N{LATIN SMALL LETTER E WITH ACUTE}"sv) == 1 );

  auto sv = u8"ee\N{COMBINING ACUTE ACCENT}e"sv;
  auto data = sv.data();
  VERIFY( uc::__field_width(sv) == 3 );
  VERIFY( uc::__truncate(sv, 3) == 3 );
  VERIFY( uc::__truncate(sv, 4) == 3 );
  VERIFY( sv == data );

  VERIFY( uc::__truncate(sv, 2) == 2 );
  VERIFY( sv == u8"ee\N{COMBINING ACUTE ACCENT}"sv );

  sv = data;
  sv.remove_prefix(1);
  VERIFY( uc::__field_width(sv) == 2 );
  VERIFY( uc::__truncate(sv, 3) == 2 );
  VERIFY( sv == data+1 );

  sv = u8"\N{REGIONAL INDICATOR SYMBOL LETTER G}"
	 "\N{REGIONAL INDICATOR SYMBOL LETTER B}"; // GB flag emoji
  data = sv.data();
  VERIFY( uc::__field_width(sv) == 1 );
  VERIFY( uc::__truncate(sv, 2) == 1 );
  VERIFY( sv == data );
  VERIFY( uc::__truncate(sv, 1) == 1 ); // Do not break inside a flag emoji.
  VERIFY( sv == data );

  sv = u8"abcd"
    "\N{REGIONAL INDICATOR SYMBOL LETTER G}" // 4 bytes
    "\N{REGIONAL INDICATOR SYMBOL LETTER B}" // 4 bytes
    "\N{DEVANAGARI LETTER KA}"               // 3 bytes
    "\N{DEVANAGARI SIGN VIRAMA}"             // 3 bytes
    "\N{DEVANAGARI LETTER RA}"               // 3 bytes
    "\N{MAN}\N{ZERO WIDTH JOINER}"           // 4+3 bytes
    "\N{WOMAN}\N{ZERO WIDTH JOINER}"         // 4+3 bytes
    "\N{GIRL}\N{ZERO WIDTH JOINER}"          // 4+3 bytes
    "\N{BOY}\N{ZERO WIDTH JOINER}"           // 4+3 bytes
    "\N{HANGUL CHOSEONG KIYEOK}"             // 3 bytes
    "\N{HANGUL CHOSEONG KIYEOK}"             // 3 bytes
    "\N{HANGUL CHOSEONG KIYEOK}"             // 3 bytes
    "\N{HANGUL CHOSEONG KIYEOK}"             // 3 bytes
    "\N{HANGUL JUNGSEONG A}"                 // 3 bytes
    "\N{HANGUL JONGSEONG KIYEOK}"            // 3 bytes
    "\N{HANGUL JONGSEONG KIYEOK}"            // 3 bytes
    "\N{HANGUL JONGSEONG KIYEOK}";           // 3 bytes

  uc::_Grapheme_cluster_view gv(sv);
  auto iter = gv.begin();
  VERIFY( iter.base() == sv.data() );
  VERIFY( *iter == U'a' );
  std::ranges::advance(iter, 3);
  VERIFY( *iter == U'd' );
  VERIFY( iter.base() == sv.data() + 3 );
  ++iter;
  VERIFY( *iter == U'\N{REGIONAL INDICATOR SYMBOL LETTER G}' );
  VERIFY( iter.base() == sv.data() + 4 );
  ++iter;
  VERIFY( *iter == U'\N{DEVANAGARI LETTER KA}' );
  VERIFY( iter.base() == sv.data() + 4 + 8 );
  ++iter;
  VERIFY( *iter == U'\N{MAN}' );
  VERIFY( iter.base() == sv.data() + 4 + 8 + 9 );
  ++iter;
  VERIFY( iter.base() == sv.data() + 4 + 8 + 9 + 28 );
  VERIFY( *iter == U'\N{HANGUL CHOSEONG KIYEOK}' );
  ++iter;
  VERIFY( iter.base() == sv.data() + 4 + 8 + 9 + 28 + 24 );
  VERIFY( iter == gv.end() );
  ++iter;
  VERIFY( iter == gv.end() );
}

constexpr void
test_pr115119()
{
  // PR 115119 Typo in _Grapheme_cluster_view::_Iterator::operator++(int)
  uc::_Grapheme_cluster_view gv(" "sv);
  auto it = std::ranges::begin(gv);
  it++;
  VERIFY( it == std::ranges::end(gv) );
}

int main()
{
  auto run_tests = []{
    test_breaks();
    test_pr115119();
    return true;
  };

  VERIFY( run_tests() );
  static_assert( run_tests() );
}
