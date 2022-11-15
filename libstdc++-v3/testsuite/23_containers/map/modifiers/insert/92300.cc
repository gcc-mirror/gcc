// { dg-do run { target c++17 } }

#include <map>
#include <cstdlib>

bool oom = false;

void* operator new(std::size_t n)
{
  if (oom)
    throw std::bad_alloc();
  return std::malloc(n);
}

void operator delete(void* p) noexcept
{
  std::free(p);
}

void operator delete(void* p, std::size_t) noexcept
{
  std::free(p);
}

int main()
{
  using std::pair;
  std::map<int, int> m;
  int i = 0;
  (void) m[i];
  oom = true;
  m.insert({i, 1});			    // insert(value_type&&)
  m.insert(pair<int, int>(i, 2));	    // insert(Pair&&)
  m.insert(pair<int&, int>(i, 3));	    // insert(Pair&&)
  m.insert(pair<int, long>(i, 4L));	    // insert(Pair&&)
  m.insert(pair<const int, long>(i, 5L));   // insert(Pair&&)
  m.insert(pair<const int&, long>(i, 6L));  // insert(Pair&&)
}
