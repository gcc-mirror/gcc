#include <istream>

// C++11 27.7.2.4 Standard basic_istream manipulators [istream.manip]
//
// LWG 415. behavior of std::ws
// std::ws is an unformatted input function.

#include <istream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

void
test01()
{
  std::wistream is(0);
  VERIFY( is.rdstate() == std::ios_base::badbit );

  is >> std::ws; // sentry should set failbit
  VERIFY( is.rdstate() & std::ios_base::failbit );
}

void
test02()
{
  __gnu_test::sync_wstreambuf buf;
  std::wistream is(&buf);

  __gnu_test::sync_wstreambuf buf_tie;
  std::wostream os_tie(&buf_tie);

  // A sentry should be constructed so is.tie()->flush() should be called.
  // The standard allows the flush to be deferred because the put area of
  // is_tie is empty, but libstdc++ doesn't defer it.
  is.tie(&os_tie);

  is >> std::ws;

  VERIFY( is.eof() );
  VERIFY( !is.fail() );
  VERIFY( ! buf.sync_called() );
  VERIFY( buf_tie.sync_called() );
}

void
test03()
{
  __gnu_test::fail_wstreambuf buf;
  std::wistream is(&buf);

  wchar_t c;
  is >> c >> std::ws;
  VERIFY( is.rdstate() == std::ios_base::badbit );

  is.clear();
  is.exceptions(std::ios_base::badbit);

  try
  {
    is >> std::ws;
    VERIFY( false );
  }
  catch (const __gnu_test::underflow_error&)
  {
    VERIFY( is.rdstate() == std::ios_base::badbit );
  }
  catch (...)
  {
    VERIFY( false );
  }
}

int main()
{
  test01();
  test02();
  test03();
}
