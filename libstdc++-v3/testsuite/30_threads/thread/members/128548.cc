// { dg-do run { target c++11 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }

// Bug 128548 - std::thread::join should check to make sure id is not self

#include <thread>
#include <system_error>
#include <testsuite_hooks.h>

int main()
{
  std::error_code ec;
  std::thread t;
  t = std::thread{ [&] {
      try
	{
	  t.join();
	}
      catch (const std::system_error& e)
	{
	  ec = e.code();
	}
  }};
  t.join();
  VERIFY( ec == std::errc::resource_deadlock_would_occur );
}
