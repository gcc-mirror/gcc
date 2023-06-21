// { dg-do run { target c++17 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }

#include <experimental/synchronized_value>
#include <testsuite_hooks.h>
#include <string>

using std::experimental::synchronized_value;

synchronized_value<std::string> s;

std::string read_value(){
  return apply([](auto& x){return x;},s);
}

void set_value(std::string const& new_val){
  apply([&](auto& x){ x = new_val; }, s);
}

void
test_single()
{
  set_value("new value");
  VERIFY( read_value() == "new value" );
}

void
test_multi()
{
  synchronized_value<int> a(1), b(2), c(3);
  int sum = apply([](auto&... ints) { return (ints++ + ...); }, a, b, c);
  VERIFY( sum == 6 );
  auto get = [](int& i) { return i; };
  VERIFY( apply(get, a) == 2 );
  VERIFY( apply(get, b) == 3 );
  VERIFY( apply(get, c) == 4 );
}

int main()
{
  test_single();
  test_multi();
}
