// { dg-compile }
// { dg-additional-options "-Wnull-dereference" }

#include <string>
#include <sstream>

int main()
{
  std::istringstream in("Hello, world");
  std::istreambuf_iterator<char> it(in), end;
  std::string ss(it, end);
  return 0;
}

// { dg-bogus "null pointer dereference" "" { target *-*-* } 0 }
