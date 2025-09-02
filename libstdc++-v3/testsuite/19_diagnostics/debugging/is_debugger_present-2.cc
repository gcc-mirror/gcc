// { dg-do run { target c++26 } }
// { dg-options "-lstdc++exp" }
// { dg-require-cpp-feature-test __cpp_lib_debugging }
// { dg-xfail-run-if "no replaceable functions on AIX" { powerpc-ibm-aix* } }

// P2810R4 is_debugger_present is_replaceable

#include <debugging>
#include <testsuite_hooks.h>

bool called = false;

bool std::is_debugger_present() noexcept { called = true; return true; }

int main()
{
  VERIFY( std::is_debugger_present() );
  VERIFY( called );
}
