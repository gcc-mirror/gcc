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
      std::string s = "This is the first runtime error";
      throw T(s);
    }
  catch (const T &x)
    {
      VERIFY( std::string(x.what()) == "This is the first runtime error" );
    }
  try
    {
      throw T("This is the second runtime error");
    }
  catch (const std::runtime_error &x)
    {
      VERIFY( std::string(x.what()) == "This is the second runtime error" );
    }
  std::string s = "This is the third runtime error";
  T l(s);
  try
    {
      throw T(l);
    }
  catch (const std::runtime_error &x)
    {
      VERIFY( std::string(x.what()) == "This is the third runtime error" );
    }
  VERIFY( std::string(l.what()) == "This is the third runtime error" );
  s = "This is the fourth runtime error";
  l = T(s);
  try
    {
      throw T(std::move(l));
    }
  catch (const T &x)
    {
      VERIFY( std::string(x.what()) == "This is the fourth runtime error" );
    }
  T l2(s);
  l2 = T("This is the fifth runtime error");
  VERIFY( std::string(l2.what()) == "This is the fifth runtime error" );
  T l3("This is the sixth runtime error");
  VERIFY( std::string(l3.what()) == "This is the sixth runtime error" );
  l3 = l2;
  VERIFY( std::string(l2.what()) == "This is the fifth runtime error" );
  VERIFY( std::string(l3.what()) == "This is the fifth runtime error" );
  l3 = T("This is the seventh runtime error");
  l2 = std::move(l3);
  VERIFY( std::string(l2.what()) == "This is the seventh runtime error" );
  return true;
}

static_assert(test01<std::runtime_error>());
static_assert(test01<std::range_error>());
static_assert(test01<std::overflow_error>());
static_assert(test01<std::underflow_error>());

int main(void)
{
  test01<std::runtime_error>();
  test01<std::range_error>();
  test01<std::overflow_error>();
  test01<std::underflow_error>();
}
