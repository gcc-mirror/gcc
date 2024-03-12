// { dg-do run { target c++23 } }

#include <optional>
#include <testsuite_hooks.h>

constexpr bool
test_or_else()
{
  std::optional<int> o;
  auto&& r = o.or_else([]() -> std::optional<int> { return {303}; });
  VERIFY( !o );
  VERIFY( *r == 303 );
  static_assert( std::is_same_v<decltype(r), std::optional<int>&&> );

  o = 808;
  const std::optional<int> tr = 909;
  auto&& r2 = o.or_else([&]() -> const auto& { return tr; });
  static_assert( std::is_same_v<decltype(r2), std::optional<int>&&> );
  VERIFY( r2 == o );

  return true;
}

static_assert( test_or_else() );

constexpr bool
test_move()
{
  struct X
  {
    constexpr X() { }
    constexpr X(const X&) { copied = true; }
    constexpr X(X&& x) { moved = true; x.gone = true; }

    bool copied = false;
    bool moved = false;
    bool gone = false;
  };

  std::optional<X> o(std::in_place);

  auto f = []{ return std::optional<X>{}; };
  VERIFY( o.or_else(f)->copied );
  VERIFY( ! o->gone );

  VERIFY( std::move(o).or_else(f)->moved );
  VERIFY( o->gone );

  struct move_only
  {
    constexpr move_only() { }
    constexpr move_only(move_only&&) { }
  };

  std::optional<move_only> mo;
  // doesn't require copy
  std::move(mo).or_else([] { return std::optional<move_only>{}; });

  return true;
}

static_assert( test_move() );

constexpr bool
test_call()
{
  struct F
  {
    constexpr std::optional<int> operator()() & { return {1}; }
    constexpr std::optional<int> operator()() && { return {2}; }
    constexpr std::optional<int> operator()() const & { return {3}; };
    constexpr std::optional<int> operator()() const && { return {4}; }
  };

  std::optional<int> o;
  F f;

  VERIFY( *o.or_else(f) == 1 );
  VERIFY( *std::move(o).or_else(f) == 1 );

  VERIFY( *o.or_else(std::move(f)) == 2 );
  VERIFY( *std::move(o).or_else(std::move(f)) == 2 );

  const F& cf = f;

  VERIFY( *o.or_else(cf) == 3 );
  VERIFY( *std::move(o).or_else(cf) == 3 );

  VERIFY( *o.or_else(std::move(cf)) == 4 );
  VERIFY( *std::move(o).or_else(std::move(cf)) == 4 );

  return true;
}

static_assert( test_call() );

int main()
{
  test_or_else();
  test_move();
  test_call();
}
