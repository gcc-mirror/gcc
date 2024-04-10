// { dg-do compile { target c++20 } }
#include <format>

using std::__unicode::__charset_alias_match;
static_assert( __charset_alias_match("UTF-8", "utf8") == true );
static_assert( __charset_alias_match("UTF-8", "u.t.f-008") == true );
static_assert( __charset_alias_match("UTF-8", "utf-80") == false );
static_assert( __charset_alias_match("UTF-8", "ut8") == false );

static_assert( __charset_alias_match("iso8859_1", "ISO-8859-1") == true );

static_assert( __charset_alias_match("", "") == true );
static_assert( __charset_alias_match("", ".") == true );
static_assert( __charset_alias_match("--", "...") == true );
static_assert( __charset_alias_match("--a", "a...") == true );
static_assert( __charset_alias_match("--a010", "a..10.") == true );
static_assert( __charset_alias_match("--a010", "a..1.0") == false );
static_assert( __charset_alias_match("aaaa", "000.00.0a0a)0aa...") == true );
