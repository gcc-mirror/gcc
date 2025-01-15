// { dg-do run }

// 4188. ostream::sentry destructor should handle exceptions

#include <ostream>
#include <streambuf>
#include <testsuite_hooks.h>

struct bad_streambuf : std::streambuf
{
  int sync() { return -1; }
};

void
test_returns_error()
{
  bad_streambuf buf;
  std::ostream out(&buf);
  out.setf(std::ios::unitbuf);
  out.exceptions(std::ios::badbit);
  out.write("", 0); // constructs sentry
  VERIFY( out.bad() );
}

struct exceptionally_bad_streambuf : std::streambuf
{
  int sync() { throw std::ios::failure("unsyncable"); }
};

void
test_throws()
{
  exceptionally_bad_streambuf buf;
  std::ostream out(&buf);
  out.setf(std::ios::unitbuf);
  out.write("", 0); // constructs sentry
  VERIFY( out.bad() );

  // Repeat with badbit in exceptions mask
  out.clear();
  out.exceptions(std::ios::badbit);
  out.write("", 0); // constructs sentry
  VERIFY( out.bad() );
}

int main()
{
  test_returns_error();
  test_throws();
}
