// { dg-options "-ltbb" }
// { dg-do run { target c++17 } }
// { dg-require-effective-target tbb_backend }

#include <numeric>
#include <execution>
#include <testsuite_hooks.h>

struct Mint
{
  Mint(int i = 0) : val(i) { }

  Mint(Mint&& m) : val(m.val) { m.val = -1; }

  Mint& operator=(Mint&& m)
  {
    val = m.val;
    m.val = -1;
    return *this;
  }

  operator int() const
  {
    VERIFY(val >= 0); // Check we don't read value of a moved-from instance.
    return val;
  }

  friend Mint operator+(const Mint& lhs, const Mint& rhs)
  { return Mint(lhs.val + rhs.val); }

private:
  int val;
};

void
test_reduce()
{
  Mint input[]{1, 2, 3};
  Mint m = std::reduce(std::execution::seq, input, input+3);
  VERIFY( static_cast<int>(m) == 6 );

  m = std::reduce(std::execution::seq, input, input+3, Mint(100));
  VERIFY( static_cast<int>(m) == 106 );

  m = std::reduce(std::execution::seq, input, input+3, Mint(200),
		  std::plus<>{});
  VERIFY( static_cast<int>(m) == 206 );
}

void
test_transform_reduce()
{
}

void
test_exclusive_scan()
{
  const int input[]{10, 20, 30};
  int output[3];
  std::exclusive_scan(std::execution::seq, input, input+3, output, Mint(5),
		      std::plus<int>{});
  VERIFY( output[0] == 5 );
  VERIFY( output[1] == 15 );
  VERIFY( output[2] == 35 );
}

void
test_inclusive_scan()
{
}

void
test_transform_exclusive_scan()
{
}

void
test_transform_inclusive_scan()
{
}

int main()
{
  test_reduce();
  test_transform_reduce();
  test_exclusive_scan();
  test_inclusive_scan();
  test_transform_exclusive_scan();
  test_transform_inclusive_scan();
}
