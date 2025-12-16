// { dg-do compile { target c++26 } }
// { dg-require-effective-target cxx11_abi }

#include <string>
#include <stdexcept>
#include <testsuite_hooks.h>

template <typename T>
constexpr bool test01()
{
  try
    {
      std::string s = "This is the first logic error";
      throw T(s);
    }
  catch (const T &x)
    {
      VERIFY( std::string(x.what()) == "This is the first logic error" );
    }
  try
    {
      throw T("This is the second logic error");
    }
  catch (const std::logic_error &x)
    {
      VERIFY( std::string(x.what()) == "This is the second logic error" );
    }
  std::string s = "This is the third logic error";
  T l(s);
  try
    {
      throw T(l);
    }
  catch (const std::logic_error &x)
    {
      VERIFY( std::string(x.what()) == "This is the third logic error" );
    }
  VERIFY( std::string(l.what()) == "This is the third logic error" );
  s = "This is the fourth logic error";
  l = T(s);
  try
    {
      throw T(std::move(l));
    }
  catch (const T &x)
    {
      VERIFY( std::string(x.what()) == "This is the fourth logic error" );
    }
  T l2(s);
  l2 = T("This is the fifth logic error");
  VERIFY( std::string(l2.what()) == "This is the fifth logic error" );
  T l3("This is the sixth logic error");
  VERIFY( std::string(l3.what()) == "This is the sixth logic error" );
  l3 = l2;
  VERIFY( std::string(l2.what()) == "This is the fifth logic error" );
  VERIFY( std::string(l3.what()) == "This is the fifth logic error" );
  l3 = T("This is the seventh logic error");
  l2 = std::move(l3);
  VERIFY( std::string(l2.what()) == "This is the seventh logic error" );
  return true;
}

static_assert(test01<std::logic_error>());
static_assert(test01<std::domain_error>());
static_assert(test01<std::invalid_argument>());
static_assert(test01<std::length_error>());
static_assert(test01<std::out_of_range>());

int main(void)
{
  test01<std::logic_error>();
  test01<std::domain_error>();
  test01<std::invalid_argument>();
  test01<std::length_error>();
  test01<std::out_of_range>();
}
