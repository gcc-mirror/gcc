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
  std::map<int, int> m;
  int i = 0;
  (void) m[i];
  oom = true;
  m.emplace(i, 1);
  m.emplace(i, 2L);
  const int c = 3;
  m.emplace(i, c);
  m.emplace((long)i, 4);
}
