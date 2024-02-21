// { dg-additional-options "-lstdc++exp" { target { *-*-mingw* } } }
// { dg-do run { target c++23 } }
// { dg-require-fileio "" }

#include <print>
#include <cstdio>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

void
test_print_default()
{
  std::print("H{}ll{}, {}!", 3, 0, "world");
  // { dg-output "H3ll0, world!" }
}

void
test_println_default()
{
  std::println("I walk the line");
  // { dg-output "I walk the line\r?\n" }
}

void
test_print_file()
{
  __gnu_test::scoped_file f;
  FILE* strm = std::fopen(f.path.string().c_str(), "w");
  VERIFY( strm );
  std::print(strm, "File under '{}' for {}", 'O', "OUT OF FILE");
  std::fclose(strm);

  std::ifstream in(f.path);
  std::string txt(std::istreambuf_iterator<char>(in), {});
  VERIFY( txt == "File under 'O' for OUT OF FILE" );
}

void
test_println_file()
{
  __gnu_test::scoped_file f;
  FILE* strm = std::fopen(f.path.string().c_str(), "w");
  VERIFY( strm );
  std::println(strm, "{} Lineman was a song I once heard", "Wichita");
  std::fclose(strm);

  std::ifstream in(f.path);
  std::string txt(std::istreambuf_iterator<char>(in), {});
  VERIFY( txt == "Wichita Lineman was a song I once heard\n" );
}

void
test_print_raw()
{
  __gnu_test::scoped_file f;
  FILE* strm = std::fopen(f.path.string().c_str(), "w");
  VERIFY( strm );
  std::print(strm, "{}", '\xa3'); // Not a valid UTF-8 string.
  std::fclose(strm);

  std::ifstream in(f.path);
  std::string txt(std::istreambuf_iterator<char>(in), {});
  // Invalid UTF-8 should be written out unchanged if the stream is not
  // connected to a tty:
  VERIFY( txt == "\xa3" );
}

void
test_vprint_nonunicode()
{
  std::vprint_nonunicode_buffered("{0} in \xc0 {0} out\n",
      std::make_format_args("garbage"));
  // { dg-output "garbage in . garbage out\r?\n" }
  std::vprint_nonunicode_buffered(stdout, "{0} in \xc3 {0} out\n",
      std::make_format_args("junk"));
  // { dg-output "junk in . junk out\r?\n" }
  std::vprint_nonunicode(stdout, "{0} in \xc2 {0} out\n",
      std::make_format_args("trash"));
  // { dg-output "trash in . trash out\r?\n" }

}

#ifdef __cpp_exceptions
void
test_errors()
{
  try
  {
    std::print(stdin, "{}", "nope");
    VERIFY(false);
  }
  catch (const std::system_error&)
  {
  }
}

struct ThrowOnFormat
{};

template<typename CharT>
struct std::formatter<ThrowOnFormat, CharT>
{
  constexpr typename std::basic_format_parse_context<CharT>::iterator
  parse(const std::basic_format_parse_context<CharT>& pc) const
  { return pc.begin(); }

  template<typename Out>
  typename std::basic_format_context<Out, CharT>::iterator
  format(ThrowOnFormat, const std::basic_format_context<Out, CharT>&) const
  { throw ThrowOnFormat{}; }
};

void
test_buffered()
{
  __gnu_test::scoped_file f;
  FILE* strm = std::fopen(f.path.string().c_str(), "w");
  VERIFY( strm );
  try
  {
    std::string s = "Test";
    ThrowOnFormat tf;
    std::vprint_unicode_buffered(strm, "{} {} {} {}", std::make_format_args(s, s, s, tf));
    VERIFY(false);
  }
  catch (ThrowOnFormat)
  { }
  std::fclose(strm);

  std::ifstream in(f.path);
  std::string txt(std::istreambuf_iterator<char>(in), {});
  VERIFY( txt.empty() );
}
#endif

int main()
{
  test_print_default();
  test_println_default();
  test_print_file();
  test_println_file();
  test_print_raw();
  test_vprint_nonunicode();
#ifdef __cpp_exceptions
  test_errors();
  test_buffered();
#endif
}
