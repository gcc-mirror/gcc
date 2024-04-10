// { dg-do compile { target c++20 } }

#include <format> // includes <bits/unicode.h>
#include <string_view>
#include <ranges>
#include <testsuite_hooks.h>

namespace uc = std::__unicode;
using namespace std::string_view_literals;

constexpr char32_t riA = U'\N{REGIONAL INDICATOR SYMBOL LETTER A}';
constexpr char32_t riZ = U'\N{REGIONAL INDICATOR SYMBOL LETTER Z}';

static_assert( uc::__field_width(U'\0') == 1 );
static_assert( uc::__field_width(U'1') == 1 );
static_assert( uc::__field_width(U'a') == 1 );
static_assert( uc::__field_width(riA) == 1 );
static_assert( uc::__field_width(U'\N{OBLIQUE HYPHEN}') == 1 );
static_assert( uc::__field_width(U'\N{CIRCLED NUMBER EIGHTY ON BLACK SQUARE}')
    == 1 );

static_assert( uc::__field_width(U'\N{SESQUIQUADRATE}') == 1 );
static_assert( uc::__field_width(U'\N{SOCCER BALL}') == 2 );
static_assert( uc::__field_width(U'\N{BASEBALL}') == 2 );
static_assert( uc::__field_width(U'\N{SQUARED KEY}') == 1 );
static_assert( uc::__field_width(U'\N{BLACK DRAUGHTS KING}') == 1 );
static_assert( uc::__field_width(U'\N{SNOWMAN WITHOUT SNOW}') == 2 );

static_assert( uc::__field_width(U'\N{IDEOGRAPHIC SPACE}') == 2 );
static_assert( uc::__field_width(U'\N{IDEOGRAPHIC COMMA}') == 2 );
static_assert( uc::__field_width(U'\N{CIRCLED IDEOGRAPH ONE}') == 2 );

// EastAsianWidth.txt says these are normal width, but C++ says width 2:
static_assert( uc::__field_width(U'\u4DC0') == 2 );
static_assert( uc::__field_width(U'\u4DC1') == 2 );
static_assert( uc::__field_width(U'\u4DFF') == 2 );
// EastAsianWidth.txt says W and C++ says 2:
static_assert( uc::__field_width(U'\U0001F300') == 2 );
static_assert( uc::__field_width(U'\U0001F320') == 2 );
// EastAsianWidth.txt says N but C++ says 2:
static_assert( uc::__field_width(U'\U0001F321') == 2 );
static_assert( uc::__field_width(U'\U0001F5FA') == 2 );
// EastAsianWidth.txt says W and C++ says 2:
static_assert( uc::__field_width(U'\U0001F5FF') == 2 );
static_assert( uc::__field_width(U'\U0001F600') == 2 );

static_assert( uc::__field_width(U'\U0001F900') == 2 );
static_assert( uc::__field_width(U'\U0001F90B') == 2 );
static_assert( uc::__field_width(U'\U0001F90C') == 2 );
static_assert( uc::__field_width(U'\U0001F93B') == 2 );
static_assert( uc::__field_width(U'\U0001F9FF') == 2 );
static_assert( uc::__field_width(U'\U0001FA00') == 1 );
static_assert( uc::__field_width(U'\U0001FA69') == 1 );
static_assert( uc::__field_width(U'\U0001FA70') == 2 );
static_assert( uc::__field_width(U'\U0001FAF8') == 2 );
static_assert( uc::__field_width(U'\U0001FAF9') == 1 );

using enum uc::_Gcb_property;
static_assert( uc::__grapheme_cluster_break_property(U'\0') == _Gcb_Control );
static_assert( uc::__grapheme_cluster_break_property(U'a') == _Gcb_Other );
static_assert( uc::__grapheme_cluster_break_property(riA)
    == _Gcb_Regional_Indicator );
static_assert( uc::__grapheme_cluster_break_property(riZ)
    == _Gcb_Regional_Indicator );
static_assert( uc::__grapheme_cluster_break_property(riA - 1) == _Gcb_Other );
static_assert( uc::__grapheme_cluster_break_property(riZ + 1) == _Gcb_Other );
static_assert( uc::__grapheme_cluster_break_property(U'\uD788') == _Gcb_LV );
static_assert( uc::__grapheme_cluster_break_property(U'\uD7A3') == _Gcb_LVT );
static_assert( uc::__grapheme_cluster_break_property(U'\u200D') == _Gcb_ZWJ );
static_assert( uc::__grapheme_cluster_break_property(U'\U0001D16D')
    == _Gcb_SpacingMark );
static_assert( uc::__grapheme_cluster_break_property(U'\U0001D16E')
    == _Gcb_Extend );
static_assert( uc::__grapheme_cluster_break_property(U'\U000E01EF')
    == _Gcb_Extend );
static_assert( uc::__grapheme_cluster_break_property(U'\U000E01F0')
    == _Gcb_Control );
static_assert( uc::__grapheme_cluster_break_property(U'\U000E0FFF')
    == _Gcb_Control );
static_assert( uc::__grapheme_cluster_break_property(U'\U000E1000')
    == _Gcb_Other );

static_assert( uc::__incb_property(U'\0') == uc::_InCB{0} );
static_assert( uc::__incb_property(U'a') == uc::_InCB{0} );
static_assert( uc::__incb_property(U'\N{DEVANAGARI LETTER KA}')
		== uc::_InCB::_Consonant );
static_assert( uc::__incb_property(U'\N{DEVANAGARI LETTER RA}')
		== uc::_InCB::_Consonant );
static_assert( uc::__incb_property(U'\N{DEVANAGARI LETTER YYA}')
		== uc::_InCB::_Consonant );
static_assert( uc::__incb_property(U'\N{DEVANAGARI LETTER YYA}' + 1)
		== uc::_InCB{0} );
static_assert( uc::__incb_property(U'\N{DEVANAGARI SIGN NUKTA}')
		== uc::_InCB::_Extend );
static_assert( uc::__incb_property(U'\N{DEVANAGARI SIGN NUKTA}' + 1)
		== uc::_InCB{0} );
static_assert( uc::__incb_property(U'\U0001E94A') == uc::_InCB::_Extend );
static_assert( uc::__incb_property(U'\U0001E94B') == uc::_InCB{0} );

static_assert( ! uc::__is_incb_linker(U'\0') );
static_assert( ! uc::__is_incb_linker(U'a') );
static_assert( uc::__is_incb_linker(U'\N{DEVANAGARI SIGN VIRAMA}') );
static_assert( ! uc::__is_incb_linker(U'\N{DEVANAGARI SIGN VIRAMA}' + 1) );
static_assert( ! uc::__is_incb_linker(U'\N{DEVANAGARI SIGN VIRAMA}' - 1) );
static_assert( ! uc::__is_incb_linker(U'\u0FFF') );
static_assert( ! uc::__is_incb_linker(U'\uFFFD') );

static_assert( ! uc::__is_extended_pictographic(U'\0') );
static_assert( ! uc::__is_extended_pictographic(U'a') );
static_assert( ! uc::__is_extended_pictographic(riA) );
static_assert( ! uc::__is_extended_pictographic(riZ) );
static_assert( ! uc::__is_extended_pictographic(U'\N{COPYRIGHT SIGN}' - 1) );
static_assert( uc::__is_extended_pictographic(U'\N{COPYRIGHT SIGN}') );
static_assert( ! uc::__is_extended_pictographic(U'\N{COPYRIGHT SIGN}' + 1) );
static_assert( ! uc::__is_extended_pictographic(U'\N{INFORMATION SOURCE}' - 1) );
static_assert( uc::__is_extended_pictographic(U'\N{INFORMATION SOURCE}') );
static_assert( ! uc::__is_extended_pictographic(U'\N{INFORMATION SOURCE}' + 1) );
static_assert( ! uc::__is_extended_pictographic(U'\N{LEFT RIGHT ARROW}' - 1) );
static_assert( uc::__is_extended_pictographic(U'\N{LEFT RIGHT ARROW}') );
static_assert( uc::__is_extended_pictographic(U'\N{LEFT RIGHT ARROW}' + 1) );
static_assert( uc::__is_extended_pictographic(U'\N{SOUTH WEST ARROW}') );
static_assert( ! uc::__is_extended_pictographic(U'\N{SOUTH WEST ARROW}' + 1) );
static_assert( uc::__is_extended_pictographic(U'\N{POSTBOX}') );
static_assert( ! uc::__is_extended_pictographic(U'\U0001EFFF') );
static_assert( uc::__is_extended_pictographic(U'\U0001F000') );
static_assert( uc::__is_extended_pictographic(U'\U0001FFFD') );
static_assert( ! uc::__is_extended_pictographic(U'\U0001FFFE') );
static_assert( ! uc::__is_extended_pictographic(U'\U0001FFFF') );
