// { dg-do run }

// 4188. ostream::sentry destructor should handle exceptions

#include <ostream>
#include <streambuf>
#include <testsuite_hooks.h>

struct bad_streambuf : std::wstreambuf
{
  int sync() { return -1; }
};

void
test_returns_error()
{
  bad_streambuf buf;
  std::wostream out(&buf);
  out.setf(std::wios::unitbuf);
  out.exceptions(std::wios::badbit);
  out.write(L"", 0); // constructs sentry
  VERIFY( out.bad() );
}

struct exceptionally_bad_streambuf : std::wstreambuf
{
  int sync() { throw std::wios::failure("unsyncable"); }
};

void
test_throws()
{
  exceptionally_bad_streambuf buf;
  std::wostream out(&buf);
  out.setf(std::wios::unitbuf);
  out.write(L"", 0); // constructs sentry
  VERIFY( out.bad() );

  // Repeat with badbit in exceptions mask
  out.clear();
  out.exceptions(std::wios::badbit);
  out.write(L"", 0); // constructs sentry
  VERIFY( out.bad() );
}

int main()
{
  test_returns_error();
  test_throws();
}
