#include <ostream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

// C++11 27.7.3.5 basic_ostream seek members [ostream.seeks]

// Verify [ostream.seeks] functions use a sentry, as per N3168.

void
test01()
{
  // Check that the sentry sets failbit when seeking on a bad stream.
  // The standard doesn't guarantee this, but it is true for libstdc++.

  std::ostream os(0);
  VERIFY( os.rdstate() == std::ios_base::badbit );

  os.tellp();
  VERIFY( os.rdstate() & std::ios_base::failbit );

  os.clear();

  os.exceptions(std::ios_base::failbit);

  try
  {
    os.clear();
    os.tellp();
    VERIFY( false );
  }
  catch (const std::ios_base::failure&)
  {
    VERIFY( os.rdstate() & std::ios_base::failbit );
  }
  catch (...)
  {
    VERIFY( false );
  }
}

void
test02()
{
  // Check that the sentry flushes a tied stream when seeking.

  __gnu_test::sync_streambuf buf;
  std::ostream os(&buf);

  __gnu_test::sync_streambuf buf_tie;
  std::ostream os_tie(&buf_tie);

  os.tie(&os_tie);

  os.tellp();

  VERIFY( ! buf.sync_called() );
  VERIFY( buf_tie.sync_called() );
}

int main()
{
  test01();
  test02();
}
