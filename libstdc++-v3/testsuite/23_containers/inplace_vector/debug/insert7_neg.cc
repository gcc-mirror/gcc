// { dg-do run { target c++26 xfail *-*-* } }

#include <memory>
#include <iterator>
#include <debug/inplace_vector>

void
test01()
{
  __gnu_debug::inplace_vector<std::unique_ptr<int>, 10> v;

  v.emplace_back(new int(0));
  v.emplace_back(new int(1));

  v.insert(begin(v) + 1,
	   make_move_iterator(begin(v)),
	   make_move_iterator(end(v)));
}

int
main()
{
  test01();
}
