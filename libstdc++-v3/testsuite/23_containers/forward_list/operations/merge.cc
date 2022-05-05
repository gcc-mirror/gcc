// { dg-do run { target c++11 } }
// C++11 23.3.4.6 Operations [forwardlist.ops]

#include <forward_list>
#include <testsuite_hooks.h>

void
test_stable()
{
  std::forward_list<double> a{1.5, 2.0, 3.5, 4.1};
  std::forward_list<double> b{1.0, 2.5, 3.0, 4.3, 4.2, 5.0};

  a.merge(b, std::less<int>{});

  // result is sorted with respect to std::less<int>, so 1.0 and 1.5 are
  // equivalent, and stability guarantee means the element from a comes first.
  const std::forward_list<double> r { 1.5, 1.0,
				      2.0, 2.5,
				      3.5, 3.0,
				      4.1, 4.3, 4.2,
				      5.0};

  VERIFY(a == r);
}

void
test_lwg3088()
{
  // LWG 3088: forward_list::merge behavior unclear when passed *this
  // PR libstdc++/103853
  std::forward_list<int> c1{ 1, 2, 3 };
  const std::forward_list<int> c2 = c1;
  c1.merge(c1);
  VERIFY( c1 == c2 );
  c1.merge(c1, std::less<long>{});
  VERIFY( c1 == c2 );
  c1.merge(std::move(c1));
  VERIFY( c1 == c2 );
  c1.merge(std::move(c1), std::less<long>{});
  VERIFY( c1 == c2 );
}

int
main()
{
  test_stable();
  test_lwg3088();
}
