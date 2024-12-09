// { dg-do compile }

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

#if __cplusplus >= 201103L
static_assert( std::is_trivially_default_constructible<X>::value, "" );
static_assert( std::is_trivially_copyable<X>::value, "" );
#endif

void test01_pr102064()
{
  unsigned char buf[sizeof(X)];
  X* addr = reinterpret_cast<X*>(buf);
  Y y;
  std::uninitialized_fill(addr, addr + 1, y);
}

#if __cplusplus >= 201103L
struct Z
{
  Z() = default;
  Z(int) { }
  Z(const Z&) = default;
  Z& operator=(const Z&) = default;
  Z& operator=(int) = delete;
};

static_assert( std::is_trivially_default_constructible<Z>::value, "" );
static_assert( std::is_trivially_copyable<Z>::value, "" );

void test02_pr102064()
{
  unsigned char buf[sizeof(Z)];
  Z* addr = reinterpret_cast<Z*>(buf);
  std::uninitialized_fill(addr, addr + 1, 99);
}
#endif
