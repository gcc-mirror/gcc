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

  std::ostream::pos_type pos = std::ostream::pos_type();
  os.seekp(pos);
  VERIFY( os.rdstate() & std::ios_base::failbit );

  os.clear();
  std::ostream::off_type off(5);
  os.seekp(off, std::ios_base::cur);
  VERIFY( os.rdstate() & std::ios_base::failbit );

  os.clear();
  os.exceptions(std::ios_base::failbit);

  try
  {
    os.clear();
    os.seekp(pos);
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

  try
  {
    os.clear();
    os.seekp(off, std::ios_base::cur);
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

  {
    __gnu_test::sync_streambuf buf;
    std::ostream os(&buf);

    __gnu_test::sync_streambuf buf_tie;
    std::ostream os_tie(&buf_tie);

    os.tie(&os_tie);

    std::ostream::pos_type pos = std::ostream::pos_type();
    os.seekp(pos);

    VERIFY( ! buf.sync_called() );
    VERIFY( buf_tie.sync_called() );
  }

  {
    __gnu_test::sync_streambuf buf;
    std::ostream os(&buf);

    __gnu_test::sync_streambuf buf_tie;
    std::ostream os_tie(&buf_tie);

    os.tie(&os_tie);

    std::ostream::off_type off(0);
    os.seekp(off, std::ios_base::cur);

    VERIFY( ! buf.sync_called() );
    VERIFY( buf_tie.sync_called() );
  }
}

int main()
{
  test01();
  test02();
}
