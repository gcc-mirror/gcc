// { dg-do run { target c++26 } }

#include <span>
#include <testsuite_hooks.h>

void
test_first()
{
  bool arr[5];
  std::span<const bool> s(arr);
  std::span<const bool> s2 = s.first(5);
  VERIFY( s2.data() == s.data() );
  std::span<const bool> s3 = s.first<5>();
  VERIFY( s3.data() == s.data() );
}

void
test_last()
{
  bool arr[5];
  std::span<const bool> s(arr);
  std::span<const bool> s2 = s.last(5);
  VERIFY( s2.data() == s.data() );
  std::span<const bool> s3 = s.last<5>();
  VERIFY( s3.data() == s.data() );
}

void
test_subspan()
{
  bool arr[5];
  std::span<const bool> s(arr);
  std::span<const bool> s2 = s.subspan(0, 5);
  VERIFY( s2.data() == s.data() );
  std::span<const bool> s3 = s.subspan<0>();
  VERIFY( s3.data() == s.data() );
  std::span<const bool> s4 = s.subspan<0, 5>();
  VERIFY( s4.data() == s.data() );
}

int main()
{
  test_first();
  test_last();
  test_subspan();
}
