// { dg-do compile { target c++20 } }

// Bug libstdc++/121913  ranges::rotate should use ranges::iter_move

#include <algorithm>

struct A { };

struct B
{
  B& operator=(const B&) = delete;
  B& operator=(const A&) const;

  operator A() const;
};

struct I
{
  using value_type = A;
  using difference_type = int;
  B operator*() const;
  B operator[](int) const;
  I& operator++();
  I operator++(int);
  I& operator--();
  I operator--(int);
  I& operator+=(int);
  I& operator-=(int);

  auto operator<=>(const I&) const = default;

  friend A iter_move(const I&);
  friend I operator+(I, int);
  friend I operator-(I, int);
  friend I operator+(int, I);
  friend int operator-(I, I);
};

static_assert( std::random_access_iterator<I> );

void
test_pr121913()
{
  std::ranges::rotate(I{}, I{}, I{});
}
