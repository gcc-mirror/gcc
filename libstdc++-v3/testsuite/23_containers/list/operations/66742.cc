// { dg-do run { target c++11 } }

#include <list>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

// PR libstdc++/66742
// abort on sorting list with custom allocator that is not stateless

template<typename List, typename Cmp = std::less<typename List::value_type>>
bool is_sorted(const List& l, Cmp cmp = {})
{
  auto it = l.begin();
  auto next = it;
  const auto end = l.end();
  if (it == end)
    return true;
  while (++next != end)
    if (cmp(*next, *it))
      return false;
    else
      it = next;
  return true;
}

void
test01()
{
  using Alloc = __gnu_test::uneq_allocator<int>;
  Alloc a1(1);
  std::list<int, Alloc> l(a1);
  for (int i = 0; i < 1000; ++i)
  {
    l.push_front(i);
    l.push_back(i + (i % 3));
  }
  const auto orig = l;

  l.sort();
  VERIFY( is_sorted(l) );
  l.sort();
  VERIFY( is_sorted(l) );

  l = orig;
  l.sort(std::less<int>());
  VERIFY( is_sorted(l) );
  l.sort(std::greater<int>());
  VERIFY( is_sorted(l, std::greater<int>()) );
}

void
test02()
{
  // The standard doesn't require comparisons to be const-correct.
  // The initial fix for PR 66742 caused a regression here.

  struct X
  {
    bool operator<(X&) /* non-const */ { return false; }
  };

  struct Cmp
  {
    bool operator()(X&, X&) /* non-const */ { return false; }
  };

  std::list<X> l;
  l.sort();
  Cmp c;
  l.sort(c);
}

int
main()
{
  test01();
  test02();
}
