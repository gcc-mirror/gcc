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
  std::vprint_nonunicode("{0} in \xc0 {0} out\n",
      std::make_format_args("garbage"));
  // { dg-output "garbage in . garbage out" }
}

void
test_errors()
{
#ifdef __cpp_exceptions
  try
  {
    std::print(stdin, "{}", "nope");
    VERIFY(false);
  }
  catch (const std::system_error&)
  {
  }
#endif
}

int main()
{
  test_print_default();
  test_println_default();
  test_print_file();
  test_println_file();
  test_print_raw();
  test_vprint_nonunicode();
  test_errors();
}
