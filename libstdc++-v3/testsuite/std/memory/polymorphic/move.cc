// { dg-do run { target c++26 } }

#include <memory>
#include <scoped_allocator>
#include <utility>
#include <vector>
#include <optional>

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
using Polymorphic = std::polymorphic<Base, tracker_allocator<Base>>;
const Polymorphic val(std::in_place_type<Derived>, 1, 2, 3);

void
verifyNoAllocations()
{
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

void
test_ctor()
{
  std::optional<Polymorphic> src;
  auto make = [&src] -> Polymorphic&& {
    src.emplace(val);
    Counter::reset();
    return std::move(*src);
  };

  Polymorphic i1(make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );
  verifyNoAllocations();

  Polymorphic i2(std::allocator_arg, {}, make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i2 == *val );
  verifyNoAllocations();
}

void
test_assign()
{
  std::optional<Polymorphic> src;
  auto make = [&src] -> Polymorphic&& {
    src.emplace(val);
    Counter::reset();
    return std::move(*src);
  };

  Polymorphic i1(std::in_place_type<Derived>);

  i1 = make();
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() >= sizeof(Derived) );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 1 );

  auto(std::move(i1));
  i1 = make();
  VERIFY( *i1 == *val );
  VERIFY( src->valueless_after_move() );
  verifyNoAllocations();
}

void
test_swap()
{
  const Polymorphic val1(std::in_place_type<Derived>, 1, 2, 3);
  const Polymorphic val2(std::in_place_type<Derived>, 2, 4, 6);

  Polymorphic i1(val1);
  Polymorphic i2(val2);
  Counter::reset();
  i1.swap(i2);
  VERIFY( *i2 == *val1 );
  VERIFY( *i1 == *val2 );
  verifyNoAllocations();

  auto(std::move(i1));
  
  Counter::reset();
  i1.swap(i2);
  VERIFY( *i1 == *val1 );
  VERIFY( i2.valueless_after_move() );
  verifyNoAllocations();
}

void
test_valueless()
{
  auto e = [] {
    Polymorphic res(std::in_place_type<Derived>);
    auto(std::move(res));
    Counter::reset();
    return res;
  };

  Polymorphic i1(e());
  VERIFY( i1.valueless_after_move() );
  verifyNoAllocations();

  Polymorphic i2(std::allocator_arg, {}, e());
  VERIFY( i2.valueless_after_move() );
  verifyNoAllocations();

  Polymorphic i3(val);
  i3 = e();
  VERIFY( Counter::get_allocation_count() ==  0 );
  VERIFY( Counter::get_deallocation_count() >= sizeof(Derived) );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 1 );

  i3 = e();
  verifyNoAllocations();
}

constexpr void
test_constexpr()
{
  using Polymorphic = std::polymorphic<Base, __gnu_test::uneq_allocator<Base>>;
  const Polymorphic val(std::in_place_type<Derived>, 1, 2, 3);

  std::optional<Polymorphic> src;
  auto make = [&src, &val] -> Polymorphic&& {
    src.emplace(val);
    return std::move(*src);
  };

  Polymorphic i1(make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );

  Polymorphic i2(std::allocator_arg, {}, make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i2 == *val );

  i1 = make();
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );

  auto(std::move(i1));
  i1 = make();
  VERIFY( *i1 == *val );
  VERIFY( src->valueless_after_move() );

  const Polymorphic val1(std::in_place_type<Derived>, 1, 2, 3);
  const Polymorphic val2(std::in_place_type<Derived>, 2, 4, 6);

  Polymorphic s1(val1);
  Polymorphic s2(val2);
  s1.swap(s2);
  VERIFY( *s2 == *val1 );
  VERIFY( *s1 == *val2 );

  auto(std::move(s1));
  s1.swap(s2);
  VERIFY( *s1 == *val1 );
  VERIFY( s2.valueless_after_move() );

  auto e = [] {
    Polymorphic res(std::in_place_type<Derived>);
    auto(std::move(res));
    return res;
  };

  Polymorphic e1(e());
  VERIFY( e1.valueless_after_move() );

  Polymorphic e2(std::allocator_arg, {}, e());
  VERIFY( e2.valueless_after_move() );

  Polymorphic e3(val);
  e3 = e();
  e3 = e();
}

int main()
{
  test_ctor();
  test_assign();
  test_swap();
  test_valueless();
  test_constexpr();

  static_assert([] {
    test_constexpr();
    return true;
  }());
}
