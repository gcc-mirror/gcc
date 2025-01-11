// { dg-additional-options "-lstdc++exp" { target { *-*-mingw* } } }
// { dg-do run { target c++23 } }
// { dg-require-fileio "" }

#include <print>
#include <cstdio>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

void
test_println_blank()
{
  std::print("1");
  std::println();
  std::println("2");
  // { dg-output "1\r?\n2" }
}

void
test_println_blank_file()
{
  __gnu_test::scoped_file f;
  FILE* strm = std::fopen(f.path.string().c_str(), "w");
  VERIFY( strm );
  std::println(strm);
  std::fclose(strm);

  std::ifstream in(f.path);
  std::string txt(std::istreambuf_iterator<char>(in), {});
  VERIFY( txt == "\n" );
}

void
test_errors()
{
#ifdef __cpp_exceptions
  try
  {
    std::println(stdin);
    VERIFY(false);
  }
  catch (const std::system_error&)
  {
  }
#endif
}

int main()
{
  test_println_blank();
  test_println_blank_file();
  test_errors();
}
