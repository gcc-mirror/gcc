// { dg-do run { target c++26 } }
// { dg-require-effective-target hosted }

#include <functional>
#include <testsuite_hooks.h>

using std::copyable_function;

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
  std::copyable_function<const F::Counters&() const> m1(f);
  VERIFY( m1().copy == 1 );
  VERIFY( m1().move == 0 );

  // This will copy construct a new target object
  auto m2 = m1;
  VERIFY( m1 != nullptr && m2 != nullptr );
  VERIFY( m2().copy == 2 );
  VERIFY( m2().move == 0 );

  m1 = m2;
  VERIFY( m1 != nullptr && m2 != nullptr );
  VERIFY( m1().copy == 3 );
  VERIFY( m1().move == 1 ); // Copies object first and then swaps

  m1 = m1;
  VERIFY( m1 != nullptr && m2 != nullptr );
  VERIFY( m1().copy == 4 );
  VERIFY( m1().move == 2 ); // Copies object first and then swaps

  m2 = f;
  VERIFY( m2().copy == 1 );
  VERIFY( m2().move == 1 ); // Copy construct target object, then swap into m2.
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
  std::copyable_function<F::Counters() const> m1(f);
  VERIFY( m1().copy == 1 );
  VERIFY( m1().move == 0 );

  // The target object is on the heap, but we need to allocate new one
  auto m2 = m1;
  VERIFY( m1 != nullptr && m2 != nullptr );
  VERIFY( m2().copy == 2 );
  VERIFY( m2().move == 0 );

  m1 = m2;
  VERIFY( m1 != nullptr && m2 != nullptr );
  VERIFY( m1().copy == 3 );
  VERIFY( m1().move == 0 );

  m1 = m1;
  VERIFY( m1 != nullptr && m2 != nullptr );
  VERIFY( m1().copy == 4 );
  VERIFY( m1().move == 0 );

  m2 = f;
  VERIFY( m2().copy == 1 );
  VERIFY( m2().move == 0 );
}

void
test03()
{
  // Small type with non-throwing, but not non-trivial move constructor.
  struct F
  {
    F(int i) noexcept : id(i) {}
    F(const F& f) : id(f.id)
    { if (id == 3) throw id; }
    F(F&& f) noexcept : id(f.id) {  }

    int operator()() const
    { return id; }

    int id;
  };

  std::copyable_function<int() const> m1(std::in_place_type<F>, 1);
  const std::copyable_function<int() const> m2(std::in_place_type<F>, 2);
  const std::copyable_function<int() const> m3(std::in_place_type<F>, 3);

  try
  {
    auto mc = m3;
    VERIFY( false );
  }
  catch(int i)
  {
    VERIFY( i == 3 );
  }

  m1 = m2;
  VERIFY( m1() == 2 );

  try
  {
    m1 = m3;
    VERIFY( false );
  }
  catch (int i)
  {
    VERIFY( i == 3 );
  }
  VERIFY( m1() == 2 );
}

int main()
{
  test01();
  test02();
  test03();
}
