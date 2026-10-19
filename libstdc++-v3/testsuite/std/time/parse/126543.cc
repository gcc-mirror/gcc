// { dg-do run { target c++20 } }

// Bug 126543 - chrono::parse should accept "60" seconds for a utc_time
// that is a valid leap second

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

int main()
{
  std::istringstream in;
  std::chrono::utc_seconds ut;
  std::chrono::sys_seconds st;

  in.str("2016-12-31 24:59:59"); // invalid hour
  in >> parse("%F %T", ut);
  VERIFY(in.fail());
  in.clear();

  in.str("2016-12-31 23:60:59"); // invalid minute
  in >> parse("%F %T", ut);
  VERIFY(in.fail());
  in.clear();

  in.str("2016-12-31 23:59:60"); // valid leap second
  in >> parse("%F %T", ut);
  VERIFY(in.good());
  in.clear();

  in.str("2016-12-31 23:58:60"); // invalid leap second
  in >> parse("%F %T", ut);
  VERIFY(in.fail());
  in.clear();

  in.str("2016-12-31 23:59:60");
  in >> parse("%F %T", st);
  VERIFY(in.fail()); // sys_time doesn't use leap seconds
  in.clear();
}
