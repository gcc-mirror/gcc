// { dg-do run { target c++23 } }

#include <string>
#include <sstream>
#include <iterator>
#include <streambuf>
#include <ranges>
#include <testsuite_hooks.h>

std::string abcs{"abcdefghijklmnopqrstuvwxyz"};
std::string twice{abcs + " " + abcs};

void
test_range()
{
  std::istringstream stream{twice};
  std::istreambuf_iterator<char> it{stream}, end;
  std::ranges::subrange range{it, end};
  std::basic_string str{std::from_range, range};
  VERIFY(str == twice);
}

void
test_sentinel()
{
  std::istringstream stream{twice};
  std::istreambuf_iterator<char> it{stream}, end;
  std::ranges::subrange range{it, std::default_sentinel};
  std::basic_string str{std::from_range, range};
  VERIFY(str == twice);
}

void
test_take()
{
  std::istringstream stream{twice};
  std::istreambuf_iterator<char> it{stream}, end;
  std::ranges::subrange range{it, std::default_sentinel};
  std::basic_string str{std::from_range,
    range | std::views::take_while([](char c){ return c != ' '; }) };
  VERIFY(str == abcs);
}


int main()
{
  test_range();
  test_sentinel();
  test_take();
}
