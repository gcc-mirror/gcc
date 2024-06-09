// { dg-do compile { target c++23 } }

// P1951R1 Default Arguments for pair's Forwarding Constructor

#include <utility>
#include <vector>
#include <string>
#include <testsuite_hooks.h>

void
test_p1951r1_example()
{
  std::pair<std::string, std::vector<std::string>> p("hello", {});
}

struct Counter
{
  constexpr Counter() = default;
  constexpr Counter(int) { }
  constexpr Counter(const Counter& c) : copies(c.copies + 1), moves(c.moves) { }
  constexpr Counter(Counter&& c) : copies(c.copies), moves(c.moves+1) { }
  int copies = 0;
  int moves = 0;
};

constexpr bool
test_count_copies()
{
  std::pair<Counter, Counter> p1(1, {});
  VERIFY( p1.first.copies == 0 && p1.second.copies == 0 );
  VERIFY( p1.first.moves == 0 && p1.second.moves == 1 );

  std::pair<Counter, Counter> p2({}, 1);
  VERIFY( p2.first.copies == 0 && p2.second.copies == 0 );
  VERIFY( p2.first.moves == 1 && p2.second.moves == 0 );

  std::pair<Counter, Counter> p3({}, {});
  VERIFY( p3.first.copies == 0 && p3.second.copies == 0 );
  VERIFY( p3.first.moves == 1 && p3.second.moves == 1 );

  return true;
}

int main()
{
  test_p1951r1_example();
  static_assert( test_count_copies() );
}
