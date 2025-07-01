// PR libstdc++/120789 - ranges::remove_if should use ranges::iter_move
// { dg-do compile { target c++20 } }

#include <algorithm>

struct A
{
  bool operator==(const A&) const;
};

struct B
{
  B(B&&) = delete;
  B& operator=(const A&) const;

  operator A() const;
  bool operator==(const B&) const;
};

struct I
{
  using value_type = A;
  using difference_type = int;
  B operator*() const;
  I& operator++();
  I operator++(int);
  bool operator==(const I&) const;
  friend A iter_move(const I&);
};

void
test01()
{
  std::ranges::subrange<I, I> r;
  auto [begin, end] = std::ranges::remove_if(r, [](auto&&) { return true; });
}
