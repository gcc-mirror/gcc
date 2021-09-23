// { dg-do compile { target c++11 } }

#include <memory>
#include <algorithm>

struct X;

struct Y
{
  operator X() const;
};

struct X
{
private:
  void operator=(const Y&);
};

Y::operator X() const { return X(); }

static_assert( std::is_trivial<X>::value, "" );

void test01_pr102064()
{
  unsigned char buf[sizeof(X)];
  X* addr = reinterpret_cast<X*>(buf);
  const Y y[1] = { };
  std::uninitialized_copy_n(y, 1, addr);
}

struct Z
{
  Z() = default;
  Z(int) { }
  Z(const Z&) = default;
  Z& operator=(const Z&) = default;
  Z& operator=(int) = delete;
};

static_assert( std::is_trivial<Z>::value, "" );

void test02_pr102064()
{
  unsigned char buf[sizeof(Z)];
  Z* addr = reinterpret_cast<Z*>(buf);
  const int i[1] = { 99 };
  std::uninitialized_copy_n(i, 1, addr);
}
