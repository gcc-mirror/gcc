// { dg-do run { target c++17 } }

#include <string>
#include <new>
#include <cstdlib>
#include <cstring>
#include <testsuite_hooks.h>

std::size_t counter = 0;

void* operator new(std::size_t n)
{
  counter += n;
  return std::malloc(n);
}

void operator delete(void* p) noexcept
{
  std::free(p);
}

void operator delete(void* p, std::size_t) noexcept
{
  std::free(p);
}

int main()
{
  const char* str = "A string that is considerably longer than the SSO buffer";

  // PR libstdc++/103919
  // basic_string(const T&, size_t, size_t) constructor is overconstrained
  counter = 0;
  std::string s(str, 2, 6);
  VERIFY( s == "string" );
#if _GLIBCXX_USE_CXX11_ABI
  // The string fits in the SSO buffer, so nothing is allocated.
  VERIFY( counter == 0 );
#else
  // The COW string allocates a string rep and 7 chars.
  VERIFY( counter < std::strlen(str) );
#endif
}
