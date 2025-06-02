// { dg-do run { target c++26 } }

#include <memory>
#include <scoped_allocator>
#include <utility>
#include <vector>

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct Base {
  friend constexpr
  bool operator==(const Base& lhs, const Base& rhs)
  { return lhs.eq(rhs); }

private:
  constexpr virtual bool
  eq(const Base& other) const = 0;
};

struct Derived : Base
{
  constexpr Derived()
   : x(0), y(0), z(0)
  { }

  constexpr Derived(int a, int b, int c)
   : x(a), y(b), z(c)
  { }
      
private:
  constexpr bool
  eq(const Base& other) const override
  { 
    if (auto op = dynamic_cast<const Derived*>(&other))
      return this->x == op->x && this->y == op->y && this->z == op->z;
    return false;
  }

  int x;
  int y;
  int z;
};

using __gnu_test::tracker_allocator;
using Counter = __gnu_test::tracker_allocator_counter;
using Polymorhic = std::polymorphic<Base, tracker_allocator<Base>>;
const Polymorhic src(std::in_place_type<Derived>, 1, 2, 3);

constexpr void
test_ctor()
{
  Counter::reset();
  Polymorhic i1(src);
  VERIFY( *i1 == *src );
  VERIFY( &*i1 != &*src );
  VERIFY( Counter::get_allocation_count() >= sizeof(Derived) );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 0 );

  Counter::reset();
  Polymorhic i2(std::allocator_arg, {}, src);
  VERIFY( *i2 == *src );
  VERIFY( &*i2 != &*src );
  VERIFY( Counter::get_allocation_count() >= sizeof(Derived) );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

constexpr void
test_assign()
{
  Counter::reset();
  Polymorhic i1(std::in_place_type<Derived>);
  const size_t holderSize = Counter::get_allocation_count();
  VERIFY( holderSize >= sizeof(Derived) );
  Counter::reset();

  i1 = src;
  VERIFY( *i1 == *src );
  VERIFY( &*i1 != &*src );
  VERIFY( Counter::get_allocation_count() == holderSize );
  VERIFY( Counter::get_deallocation_count() == holderSize );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 1 );

  auto(std::move(i1));
  Counter::reset();

  i1 = src;
  VERIFY( *i1 == *src );
  VERIFY( &*i1 != &*src );
  VERIFY( Counter::get_allocation_count() == holderSize );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

constexpr void
test_valueless()
{
  Polymorhic e(std::in_place_type<Derived>);
  auto(std::move(e));
  VERIFY( e.valueless_after_move() );

  Counter::reset();
  Polymorhic i1(e);
  VERIFY( i1.valueless_after_move() );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );

  Polymorhic i2(std::allocator_arg, {}, e);
  VERIFY( i2.valueless_after_move() );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );

  Polymorhic i3(src);
  Counter::reset();
  i3 = e;
  VERIFY( i3.valueless_after_move() );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() >= sizeof(Derived) );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 1 );

  Counter::reset();
  i3 = e;
  VERIFY( i3.valueless_after_move() );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

constexpr void
test_all()
{
  test_ctor();
  test_assign();
  test_valueless();
}

int main()
{
  test_all();

  static_assert([] {
    test_all();
    return true;
  });
}
