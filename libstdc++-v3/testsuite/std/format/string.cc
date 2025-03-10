// { dg-do run { target c++20 } }

#include <format>
#include <testsuite_hooks.h>

template<typename... Args>
bool
is_format_string_for(const char* str, Args&&... args)
{
  try {
    (void) std::vformat(str, std::make_format_args(args...));
    return true;
  } catch (const std::format_error&) {
    return false;
  }
}

template<typename... Args>
bool
is_format_string_for(const wchar_t* str, Args&&... args)
{
  try {
    (void) std::vformat(str, std::make_wformat_args(args...));
    return true;
  } catch (const std::format_error&) {
    return false;
  }
}

void
test_no_args()
{
  VERIFY( is_format_string_for("") );
  VERIFY( is_format_string_for("chars") );
  VERIFY( is_format_string_for(" The Great Escape {{}} ") );

  VERIFY( ! is_format_string_for("{") );
  VERIFY( ! is_format_string_for("}") );
  VERIFY( ! is_format_string_for("}{") );
  VERIFY( ! is_format_string_for("{{}") );
  VERIFY( ! is_format_string_for("{{{") );
  VERIFY( ! is_format_string_for("{{{{{") );
}

void
test_indexing()
{
  VERIFY( is_format_string_for("{} to {}", "a", "b") );   // automatic indexing
  VERIFY( is_format_string_for("{1} to {0}", "a", "b") ); // manual indexing
  VERIFY( ! is_format_string_for("{0} to {}", "a", "b") );  // mixed indexing
  VERIFY( ! is_format_string_for("{} to {1}", "a", "b") );  // mixed indexing

  VERIFY( is_format_string_for("{} {} {}", 1, 2, 3) );
  VERIFY( is_format_string_for("{} {} {}", 1, 2, 3, 4) );
  VERIFY( is_format_string_for("{0} {1} {2}", 1, 2, 3, 4) );
  VERIFY( is_format_string_for("{1} {2} {3}", 1, 2, 3, 4) );
  VERIFY( is_format_string_for("{3} {3} {3}", 1, 2, 3, 4) );

  VERIFY( ! is_format_string_for("{2}", 1, 2) );

  VERIFY( ! is_format_string_for("{0} {}", 1) );
  VERIFY( ! is_format_string_for("{} {0}", 1) );
}

#if __cpp_lib_format_ranges
constexpr bool escaped_strings_supported = true;
#else
constexpr bool escaped_strings_supported = false;
#endif

void
test_format_spec()
{
  VERIFY( is_format_string_for("{:}", 1) );
  VERIFY( is_format_string_for("{0:}", 1) );
  VERIFY( is_format_string_for("{2:}", 1, 2, 3) );
  VERIFY( is_format_string_for("{0:s} {0:}", "str") );
  VERIFY( is_format_string_for("{0:} {0:c}", 'c') );
  VERIFY( is_format_string_for("{0:p} {0:}", nullptr) );
  VERIFY( is_format_string_for("{:d} {:+d}", true, true) );
  VERIFY( is_format_string_for("{:0<-#03Ld}", 1) );
  VERIFY( is_format_string_for("{1:0<-#03.4Lf}", 1, 2.3) );
  VERIFY( is_format_string_for("{1:3.3f}", 1, 2.3) );
  VERIFY( is_format_string_for("{:#d}", 'c') );
  VERIFY( is_format_string_for("{:#d}", true) );
  VERIFY( is_format_string_for("{0:s} {0:?}", "str") == escaped_strings_supported );
  VERIFY( is_format_string_for("{0:} {0:?}", 'c') == escaped_strings_supported );

  // Invalid sign options.
  VERIFY( ! is_format_string_for("{:+}", "str") );
  VERIFY( ! is_format_string_for("{:+s}", "str") );
  VERIFY( ! is_format_string_for("{:+}", 'c') );
  VERIFY( ! is_format_string_for("{:+c}", 'c') );
  VERIFY( ! is_format_string_for("{:+p}", nullptr) );
  VERIFY( ! is_format_string_for("{:+}", true) );
  VERIFY( ! is_format_string_for("{:+s}", true) );
  VERIFY( ! is_format_string_for("{:+?}", "str") );
  VERIFY( ! is_format_string_for("{:+?}", 'c') );

  // Invalid alternate forms.
  VERIFY( ! is_format_string_for("{:#}", "str") );
  VERIFY( ! is_format_string_for("{:#s}", "str") );
  VERIFY( ! is_format_string_for("{:#}", 'c') );
  VERIFY( ! is_format_string_for("{:#c}", 'c') );
  VERIFY( ! is_format_string_for("{:#}", true) );
  VERIFY( ! is_format_string_for("{:#s}", true) );
  VERIFY( ! is_format_string_for("{:#}", nullptr) );
  VERIFY( ! is_format_string_for("{:#p}", nullptr) );
  VERIFY( ! is_format_string_for("{:#?}", "str") );
  VERIFY( ! is_format_string_for("{:#?}", 'c') );

  // The 0 option is not valid for charT and bool.
  VERIFY( ! is_format_string_for("{:0c}", 'c') );
  VERIFY( ! is_format_string_for("{:0s}", true) );

  // Dynamic width arg must be a standar integer type.
  VERIFY( ! is_format_string_for("{:{}d}", 1, 1.5) );
  VERIFY( ! is_format_string_for("{:{}d}", 1, true) );
  VERIFY( ! is_format_string_for("{:{}d}", 1, "str") );
  VERIFY( ! is_format_string_for("{:{}d}", 1, nullptr) );
#ifdef __SIZEOF_INT128__
  VERIFY( ! is_format_string_for("{:{}d}", 1, static_cast<__int128>(1)) );
#endif

  // Precision only valid for string and floating-point types.
  VERIFY( ! is_format_string_for("{:.3d}", 1) );
  VERIFY( ! is_format_string_for("{:3.3d}", 1) );
  VERIFY( is_format_string_for("{:3.3s}", "str") );
  VERIFY( ! is_format_string_for("{:3.3s}", 'c') );
  VERIFY( ! is_format_string_for("{:3.3p}", nullptr) );

  // Dynamic precision arg must be a standard integer type.
  VERIFY( ! is_format_string_for("{:.{}f}", 1.0, 1.5) );
  VERIFY( ! is_format_string_for("{:.{}f}", 1.0, true) );
  VERIFY( ! is_format_string_for("{:.{}f}", 1.0, "str") );
  VERIFY( ! is_format_string_for("{:.{}f}", 1.0, nullptr) );
#ifdef __SIZEOF_INT128__
  VERIFY( ! is_format_string_for("{:{}f}", 1.0, static_cast<unsigned __int128>(1)) );
#endif

  // Invalid presentation types for integers.
  VERIFY( ! is_format_string_for("{:f}", 1) );
  VERIFY( ! is_format_string_for("{:s}", 1) );
  VERIFY( ! is_format_string_for("{:g}", 1) );
  VERIFY( ! is_format_string_for("{:E}", 1) );
  VERIFY( ! is_format_string_for("{:D}", 1) );

  // Invalid presentation types for floating-point types.
  VERIFY( ! is_format_string_for("{:d}", 1.2) );
  VERIFY( ! is_format_string_for("{:b}", 1.2) );
  VERIFY( ! is_format_string_for("{:x}", 1.2) );
  VERIFY( ! is_format_string_for("{:s}", 1.2) );

  // Invalid presentation types for strings.
  VERIFY( ! is_format_string_for("{:S}", "str") );
  VERIFY( ! is_format_string_for("{:d}", "str") );

  // Maximum integer value supported for widths and precisions is USHRT_MAX.
  VERIFY( is_format_string_for("{:65535}", 1) );
  VERIFY( is_format_string_for(L"{:65535}", 1) );
  VERIFY( ! is_format_string_for("{:65536}", 1) );
  VERIFY( ! is_format_string_for(L"{:65536}", 1) );
  VERIFY( ! is_format_string_for("{:9999999}", 1) );
  VERIFY( ! is_format_string_for(L"{:9999999}", 1) );
}

void
test_pr110862()
{
  try {
    int i = 1;
    // PR libstdc++/110862 out-of-bounds read on invalid format string
    (void) std::vformat("{0:{0}", std::make_format_args(i));
    VERIFY( false );
  } catch (const std::format_error& e) {
    std::string_view what = e.what();
    VERIFY( what.find("unmatched '{'") != what.npos );
  }
}

void
test_pr110974()
{
  try {
    double d = 1.0;
    int i = 1;
    // PR libstdc++/110974 out of bounds read on invalid format string "{:{}."
    std::string_view fmt{"{:{}.0", 5}; // "0" is not part of the format string.
    (void) std::vformat(fmt, std::make_format_args(d, i));
    VERIFY( false );
  } catch (const std::format_error& e) {
    std::string_view what = e.what();
    // GCC 13.2 throws "invalid width or precision in format-spec" after
    // trying to parse the "0" past-the-end of the format string.
    // There should be an exception before even trying that:
    VERIFY( what.find("missing precision after '.'") != what.npos );
  }
}

int main()
{
  test_no_args();
  test_indexing();
  test_format_spec();
  test_pr110862();
  test_pr110974();
}
