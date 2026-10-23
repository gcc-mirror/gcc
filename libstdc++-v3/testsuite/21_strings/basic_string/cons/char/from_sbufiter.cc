// { dg-do run { target c++11 } }

#include <string>
#include <sstream>
#include <iterator>
#include <streambuf>
#include <testsuite_hooks.h>

std::string abcs{"abcdefghijklmnopqrstuvwxyz"};
std::string twice{abcs + " " + abcs};

void
test_iters()
{
  std::istringstream stream{twice};
  std::istreambuf_iterator<char> iter{stream}, end;
  std::string str{iter, end};
  VERIFY(str == twice);
}

int main()
{
  test_iters();
}
