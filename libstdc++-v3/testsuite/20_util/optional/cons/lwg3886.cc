// { dg-do compile { target c++17 } }

// LWG 3886. Monad mo' problems

#include <optional>

void
test_cons()
{
  struct MoveOnly {
    MoveOnly(int, int) { }
    MoveOnly(MoveOnly&&) { }
  };

  // The {0,0} should be deduced as MoveOnly not const MoveOnly
  [[maybe_unused]] std::optional<const MoveOnly> o({0,0});
}

struct Tracker {
  bool moved = false;
  constexpr Tracker(int, int) { }
  constexpr Tracker(const Tracker&) { }
  constexpr Tracker(Tracker&&) : moved(true) { }

  // The follow means that is_assignable<const Tracker&, U> is true:
  template<typename T> constexpr void operator=(T&&) const { }
};

#if __cpp_lib_optional >= 202106L // for constexpr assignment
void
test_assignment()
{
  constexpr bool moved = [] {
    std::optional<const Tracker> o;
    // The {0,0} should be deduced as Tracker not const Tracker:
    o = {0,0};
    // So the contained value should have been move constructed not copied:
    return o->moved;
  }();
  static_assert( moved );
}
#endif

void
test_value_or()
{
  constexpr bool moved = [] {
    std::optional<const Tracker> o;
    return o.value_or({0,0}).moved;
  }();
  static_assert( moved );

  constexpr bool moved_rval = [] {
    std::optional<const Tracker> o;
    return std::move(o).value_or({0,0}).moved;
  }();
  static_assert( moved_rval );
}
