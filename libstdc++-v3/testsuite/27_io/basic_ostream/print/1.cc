// { dg-additional-options "-lstdc++exp" { target { *-*-mingw* } } }
// { dg-do run { target c++23 } }
// { dg-require-fileio "" }

#include <ostream>
#include <spanstream>
#include <string_view>
#include <iostream>
#include <iomanip>
#include <testsuite_hooks.h>

void
test_print_ostream()
{
  char buf[64];
  std::spanstream os(buf);
  std::print(os, "File under '{}' for {}", 'O', "OUT OF FILE");
  std::string_view txt(os.span());
  VERIFY( txt == "File under 'O' for OUT OF FILE" );
}

void
test_println_ostream()
{
  char buf[64];
  std::spanstream os(buf);
  std::println(os, "{} Lineman was a song I once heard", "Wichita");
  std::string_view txt(os.span());
  VERIFY( txt == "Wichita Lineman was a song I once heard\n" );
}

void
test_print_raw()
{
  char buf[64];
  std::spanstream os(buf);
  std::print(os, "{}", '\xa3'); // Not a valid UTF-8 string.
  std::string_view txt(os.span());
  // Invalid UTF-8 should be written out unchanged if the ostream is not
  // connected to a tty:
  VERIFY( txt == "\xa3" );
}

void
test_print_no_padding()
{
  // [ostream.formatted.print] does not say this function "determines padding",
  // see https://gcc.gnu.org/pipermail/gcc-patches/2023-December/640680.html
  char buf[64];
  std::spanstream os(buf);
  os << std::setw(60) << std::setfill('?') << std::right; // should be ignored
  std::print(os, "{} Luftballons", 99);
  std::string_view txt(os.span());
  VERIFY( txt == "99 Luftballons" );
}

void
test_vprint_nonunicode()
{
  std::ostream out(std::cout.rdbuf());
  std::vprint_nonunicode(out, "{0} in \xc0 {0} out\n",
      std::make_format_args("garbage"));
  // { dg-output "garbage in . garbage out" }
}

struct brit_punc : std::numpunct<char>
{
  std::string do_grouping() const override { return "\3\3"; }
  char do_thousands_sep() const override { return ','; }
  std::string do_truename() const override { return "yes mate"; }
  std::string do_falsename() const override { return "nah bruv"; }
};

void
test_locale()
{
  struct stream_punc : std::numpunct<char>
  {
    std::string do_grouping() const override { return "\2\2"; }
    char do_thousands_sep() const override { return '~'; }
  };

  // The default C locale.
  std::locale cloc = std::locale::classic();
  // A custom locale using comma digit separators.
  std::locale bloc(cloc, new stream_punc);

  {
    std::ostringstream os;
    std::print(os, "{:L} {}", 12345, 6789);
    VERIFY(os.str() == "12345 6789");
  }
  {
    std::ostringstream os;
    std::print(os, "{}", 42);
    VERIFY(os.str() == "42");
  }
  {
    std::ostringstream os;
    os.imbue(bloc);
    std::print(os, "{:L} {}", 12345, 6789);
    VERIFY(os.str() == "1~23~45 6789");
  }
}

void
test_errors()
{
  // Failure to generate output is reported by setting badbit.
  std::stringstream in(std::ios::in);
  std::print(in, "{}", "nope"); // No exception here.
  VERIFY(in.bad());
#ifdef __cpp_exceptions
  in.clear();
  in.exceptions(std::ios::badbit);
  try
  {
    std::print(in, "{}", "nope"); // Should throw now.
    VERIFY(false);
  }
  catch (const std::ios::failure&)
  {
  }

  // An exception thrown when formatting the string is propagated
  // without setting badbit.
  std::ostringstream out;
  try
  {
    std::vprint_nonunicode(out, "{}", std::make_format_args());
    VERIFY(false);
  }
  catch (const std::format_error&)
  {
  }
  VERIFY(out.good());
#endif
}

int main()
{
  test_print_ostream();
  test_println_ostream();
  test_print_raw();
  test_print_no_padding();
  test_vprint_nonunicode();
  test_locale();
  test_errors();
}
