// { dg-do compile { target c++23 } }

// LWG 3886. Monad mo' problems

#include <expected>

void
test_constructor()
{
  struct MoveOnly {
    MoveOnly(int, int) { }
    MoveOnly(MoveOnly&&) { }
  };

  // The {0,0} should be deduced as MoveOnly not const MoveOnly
  [[maybe_unused]] std::expected<const MoveOnly, int> e({0,0});
}

struct Tracker {
  bool moved = false;
  constexpr Tracker(int, int) { }
  constexpr Tracker(const Tracker&) { }
  constexpr Tracker(Tracker&&) : moved(true) { }

  // The follow means that is_assignable<const Tracker&, U> is true:
  template<typename T> constexpr void operator=(T&&) const { }
  // This stops a copy assignment from being declared implicitly:
  void operator=(Tracker&) = delete;
};

void
test_assignment()
{
  constexpr bool moved = [] {
    std::expected<const Tracker, int> e(std::unexpect);
    // The {0,0} should be deduced as Tracker not const Tracker:
    e = {0,0};
    // So the contained value should have been move constructed not copied:
    return e->moved;
  }();
  static_assert( moved );
}

void
test_value_or()
{
  constexpr bool moved = [] {
    const std::expected<const Tracker, int> e(std::unexpect, 1);
    return e.value_or({0,0}).moved;
  }();
  static_assert( moved );

  constexpr bool moved_rval = [] {
    std::expected<const Tracker, int> e(std::unexpect, 1);
    return std::move(e).value_or({0,0}).moved;
  }();
  static_assert( moved_rval );
}
