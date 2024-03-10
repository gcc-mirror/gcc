// { dg-options "-fno-inline" }
// { dg-do run { target c++26 } }
// { dg-additional-files "filebuf_members-1.txt" }

#include <fstream>
#include <testsuite_hooks.h>

void
test01()
{
  static_assert( std::is_same_v<std::wfilebuf::native_handle_type,
				std::wofstream::native_handle_type> );
  std::wofstream f;
  f.open("filebuf_members-1.txt");
  VERIFY( f.native_handle() == f.rdbuf()->native_handle() );
}

int main()
{
  test01();
}
