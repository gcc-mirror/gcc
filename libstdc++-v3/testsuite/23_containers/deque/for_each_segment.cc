// { dg-do run }

#include <deque>
#include <algorithm>
#include <testsuite_hooks.h>

struct Finder
{
  static int call_count;

  int target;

  explicit Finder(int t) : target(t) { }

  int*
  operator()(int* first, int* last) const
  {
    ++call_count;
    return std::find(first, last, target);
  }
};

int Finder::call_count = 0;

void
test01()
{
  // A deque of 500 ints spans ~4 internal nodes (128 elements per node)
  // Node 0: elements   0 - 127
  // Node 1: elements 128 - 255
  // Node 2: elements 256 - 383
  // Node 3: elements 384 - 499
  std::deque<int> d;
  for (int i = 0; i < 500; ++i)
    d.push_back(i);

  std::deque<int>::iterator it;

  Finder::call_count = 0;
  it = std::__for_each_segment(d.begin(), d.begin() + 100, Finder(50));
  VERIFY( it == d.begin() + 50 );
  VERIFY( Finder::call_count == 1 );

  Finder::call_count = 0;
  it = std::__for_each_segment(d.begin(), d.begin() + 100, Finder(999));
  VERIFY( it == d.begin() + 100 );
  VERIFY( Finder::call_count == 1 );

  Finder::call_count = 0;
  it = std::__for_each_segment(d.begin(), d.end(), Finder(100));
  VERIFY( it == d.begin() + 100 );
  VERIFY( Finder::call_count == 1 );

  Finder::call_count = 0;
  it = std::__for_each_segment(d.begin(), d.end(), Finder(200));
  VERIFY( it == d.begin() + 200 );
  VERIFY( Finder::call_count == 2 );

  Finder::call_count = 0;
  it = std::__for_each_segment(d.begin(), d.end(), Finder(450));
  VERIFY( it == d.begin() + 450 );
  VERIFY( Finder::call_count == 4 );

  Finder::call_count = 0;
  it = std::__for_each_segment(d.begin(), d.end(), Finder(999));
  VERIFY( it == d.end() );
  VERIFY( Finder::call_count == 4 );
}

int
main()
{
  test01();
}
