// { dg-do run { target c++11 } }
// { dg-require-debug-mode "" }

// PR libstdc++/112477

#include <map>

int main()
{
  using M = std::map<int, int>;
  using I = M::iterator;

  M map{ {1, 1}, {2, 2} };

  I it1 = map.begin();
  it1 = I{};

  I it2{};
  (void)(it1 == it2);
}
