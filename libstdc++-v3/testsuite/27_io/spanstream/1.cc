// { dg-do run { target c++23 } }
// { dg-add-options no_pch }

#include <spanstream>

#ifndef __cpp_lib_spanstream
# error "Feature-test macro for spanstream missing in <spanstream>"
#elif __cpp_lib_spanstream != 202106L
# error "Feature-test macro for spanstream has wrong value in <spanstream>"
#endif

#include <testsuite_hooks.h>

using std::ispanstream;
using std::ospanstream;
using std::span;

void
test_input()
{
  // reading input from a fixed pre-arranged character buffer
  char input[] = "10 20 30";
  ispanstream is{span<char>{input}};
  int i;
  is >> i;
  VERIFY(10 == i);
  is >> i;
  VERIFY(20 == i);
  is >> i;
  VERIFY(30 == i);
  is >>i;
  VERIFY(!is);
}

void
test_output()
{
  // writing to a fixed pre-arranged character buffer
  char output[30]{}; // zero-initialize array
  ospanstream os{span<char>{output}};
  os << 10 << 20 << 30;
  auto const sp = os.span();
  VERIFY(6 == sp.size());
  VERIFY("102030" == std::string(sp.data(), sp.size()));
  VERIFY(static_cast<void*>(output) == sp.data()); // no copying of underlying data!
  VERIFY("102030" == std::string(output)); // initialization guaranteed NUL termination
}

int main()
{
  test_input();
  test_output();
}
