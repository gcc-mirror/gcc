// { dg-do run { xfail *-*-vxworks* } }
// { dg-add-options no_pch }

#include <ios>

#if __cplusplus >= 202207L
#ifndef __cpp_lib_ios_noreplace
# error "Feature-test macro for ios::noreplace missing in <ios>"
#elif __cpp_lib_ios_noreplace < 202207L
# error "Feature-test macro for ios::noreplace has wrong value in <ios>"
#endif
#endif

#include <fstream>
#include <testsuite_hooks.h>

int main()
{
#if __cpp_lib_ios_noreplace
  std::ios::openmode noreplace = std::ios::noreplace;
#else
  std::ios::openmode noreplace = std::ios::__noreplace;
#endif

  std::ofstream of("noreplace");
  VERIFY( of.is_open() );
  of.close();
  of.open("noreplace", noreplace);
  VERIFY( ! of.is_open() );
}
