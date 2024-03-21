// { dg-do run { target c++26 } }

#include <memory>
#include <testsuite_hooks.h>

struct Obj
{
  int i;
  constexpr auto operator<=>(const Obj&) const = default;
};

template<>
struct std::hash<Obj>
{
  static size_t operator()(Obj const& obj)
  { return std::hash<int>{}(obj.i);  }
};

constexpr void
test_relops()
{
  std::indirect<Obj> i1;
  VERIFY( i1 == i1 );
  VERIFY( i1 <= i1 );
  VERIFY( i1 >= i1 );

  std::indirect<Obj> i2 = std::move(i1); // make i1 valueless
  VERIFY( i1 == i1 );
  VERIFY( i2 == i2 );
  VERIFY( i2 != i1 );
  VERIFY( i1 < i2 );
  VERIFY( i2 >= i1 );

  std::indirect<Obj> i3 = std::move(i2); // make i2 valueless
  VERIFY( i2 == i1 );
  VERIFY( i2 >= i1 );
  VERIFY( i2 <= i1 );
  VERIFY( i3 > i2 );
}

constexpr void
test_comp_with_t()
{
  std::indirect<Obj> i1;
  Obj o{2};
  VERIFY( i1 != o );
  VERIFY( i1 < o );

  std::indirect<Obj> i2(Obj{2});
  VERIFY( i2 == o );
  VERIFY( i2 <= o );
  VERIFY( o <= i2 );

  std::indirect<Obj> i3 = std::move(i2); // make i2 valueless
  VERIFY( i2 != o );
  VERIFY( i2 < o );
}

void
test_hash()
{
  Obj o{5};
  std::indirect<Obj> i(o);
  VERIFY( std::hash<std::indirect<Obj>>{}(i)
	  == std::hash<Obj>{}(o) );

  auto(std::move(i)); // make i valueless
  (void)std::hash<std::indirect<Obj>>{}(i);
}

int main()
{
  test_relops();
  test_comp_with_t();
  test_hash();

  static_assert([] {
    test_relops();
    test_comp_with_t();
    return true;
  });
}
