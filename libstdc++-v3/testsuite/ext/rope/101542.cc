// { dg-do run { target c++11 } }
// PR libstdc++/101542
#include <ext/rope>
#include <testsuite_hooks.h>

template<typename T> T f(T x) { return x; }
template<typename T> T g(T x) { return std::move(x); }

int main()
{
  std::string s;
  {
    __gnu_cxx::sequence_buffer<std::string> a(s);
    {
      __gnu_cxx::sequence_buffer<std::string> b = std::move(a);
      b.push_back('h');
      b.push_back('e');
      b.push_back('l');
      b.push_back('l');
      b.push_back('o');

      __gnu_cxx::sequence_buffer<std::string> c;
      c = f(g((std::move(b))));
    }
  }
  VERIFY( s == "hello" );
}
