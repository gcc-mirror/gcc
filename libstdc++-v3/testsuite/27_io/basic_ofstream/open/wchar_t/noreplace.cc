// { dg-do run { xfail *-*-vxworks* } }
// { dg-add-options no_pch }

#include <version>

#if __cplusplus >= 202207L
#ifndef __cpp_lib_ios_noreplace
# error "Feature-test macro for ios::noreplace missing in <version>"
#elif __cpp_lib_ios_noreplace < 202207L
# error "Feature-test macro for ios::noreplace has wrong value in <version>"
#endif
#endif

#include <fstream>
#include <testsuite_hooks.h>

int main()
{
#if __cpp_lib_ios_noreplace
  std::wios::openmode noreplace = std::wios::noreplace;
#else
  std::wios::openmode noreplace = std::wios::__noreplace;
#endif

  std::wofstream of("noreplace");
  VERIFY( of.is_open() );
  of.close();
  of.open("noreplace", noreplace);
  VERIFY( ! of.is_open() );
}
