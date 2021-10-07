// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <functional>
#include <testsuite_hooks.h>

using std::move_only_function;

void
test01()
{
  // Small type with non-throwing move constructor. Not allocated on the heap.
  struct F
  {
    F() = default;
    F(const F& f) : counters(f.counters) { ++counters.copy; }
    F(F&& f) noexcept : counters(f.counters) { ++counters.move; }

    F& operator=(F&&) = delete;

    struct Counters
    {
      int copy = 0;
      int move = 0;
    } counters;

    const Counters& operator()() const { return counters; }
  };

  F f;
  std::move_only_function<const F::Counters&() const> m1(f);
  VERIFY( m1().copy == 1 );
  VERIFY( m1().move == 0 );

  // This will move construct a new target object and destroy the old one:
  auto m2 = std::move(m1);
  VERIFY( m1 == nullptr && m2 != nullptr );
  VERIFY( m2().copy == 1 );
  VERIFY( m2().move == 1 );

  m1 = std::move(m2);
  VERIFY( m1 != nullptr && m2 == nullptr );
  VERIFY( m1().copy == 1 );
  VERIFY( m1().move == 2 );

  m2 = std::move(f);
  VERIFY( m2().copy == 0 );
  VERIFY( m2().move == 2 ); // Move construct target object, then swap into m2.
  const int moves = m1().move + m2().move;
  // This will do three moves:
  swap(m1, m2);
  VERIFY( m1().copy == 0 );
  VERIFY( m2().copy == 1 );
  VERIFY( (m1().move + m2().move) == (moves + 3) );
}

void
test02()
{
  // Move constructor is potentially throwing. Allocated on the heap.
  struct F
  {
    F() = default;
    F(const F& f) noexcept : counters(f.counters) { ++counters.copy; }
    F(F&& f) noexcept(false) : counters(f.counters) { ++counters.move; }

    F& operator=(F&&) = delete;

    struct Counters
    {
      int copy = 0;
      int move = 0;
    } counters;

    Counters operator()() const noexcept { return counters; }
  };

  F f;
  std::move_only_function<F::Counters() const> m1(f);
  VERIFY( m1().copy == 1 );
  VERIFY( m1().move == 0 );

  // The target object is on the heap so this just moves a pointer:
  auto m2 = std::move(m1);
  VERIFY( m1 == nullptr && m2 != nullptr );
  VERIFY( m2().copy == 1 );
  VERIFY( m2().move == 0 );

  m1 = std::move(m2);
  VERIFY( m1 != nullptr && m2 == nullptr );
  VERIFY( m1().copy == 1 );
  VERIFY( m1().move == 0 );

  m2 = std::move(f);
  VERIFY( m2().copy == 0 );
  VERIFY( m2().move == 1 );
  const int moves = m1().move + m2().move;
  // This just swaps the pointers, so no moves:
  swap(m1, m2);
  VERIFY( m1().copy == 0 );
  VERIFY( m2().copy == 1 );
  VERIFY( (m1().move + m2().move) == moves );
}

int main()
{
  test01();
  test02();
}
