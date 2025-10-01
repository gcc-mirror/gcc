// { dg-do run { target c++26 } }

#include <memory>
#include <scoped_allocator>
#include <utility>
#include <vector>
#include <optional>

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using __gnu_test::tracker_allocator;
using Counter = __gnu_test::tracker_allocator_counter;
using Vector = std::vector<int>;
using Indirect = std::indirect<Vector, tracker_allocator<Vector>>;
const Indirect val(std::in_place, {1, 2, 3});

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
  std::optional<Indirect> src;
  auto make = [&src] -> Indirect&& {
    src.emplace(val);
    Counter::reset();
    return std::move(*src);
  };

  Indirect i1(make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );
  verifyNoAllocations();

  Indirect i2(std::allocator_arg, {}, make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i2 == *val );
  verifyNoAllocations();
}

void
test_assign()
{
  std::optional<Indirect> src;
  auto make = [&src] -> Indirect&& {
    src.emplace(val);
    Counter::reset();
    return std::move(*src);
  };

  Indirect i1;

  i1 = make();
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == sizeof(Vector) );
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
  const Indirect val1(std::in_place, {1, 2, 3});
  const Indirect val2(std::in_place, {2, 4, 6});

  Indirect i1(val1);
  Indirect i2(val2);
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
    Indirect res;
    auto(std::move(res));
    Counter::reset();
    return res;
  };

  Indirect i1(e());
  VERIFY( i1.valueless_after_move() );
  verifyNoAllocations();

  Indirect i2(std::allocator_arg, {}, e());
  VERIFY( i2.valueless_after_move() );
  verifyNoAllocations();

  Indirect i3(val);
  i3 = e();
  VERIFY( Counter::get_allocation_count() ==  0 );
  VERIFY( Counter::get_deallocation_count() == sizeof(Vector) );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 1 );

  i3 = e();
  verifyNoAllocations();
}

constexpr void
test_constexpr()
{
  using Alloc = __gnu_test::uneq_allocator<Vector>;
  using Indirect = std::indirect<Vector, Alloc>;
  const Indirect val(std::in_place, {1, 2, 3});

  std::optional<Indirect> src;
  auto make = [&src, &val] -> Indirect&& {
    src.emplace(val);
    return std::move(*src);
  };

  Indirect i1(make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );

  Indirect i2(std::allocator_arg, {}, make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i2 == *val );

  i2 = make();
  VERIFY( src->valueless_after_move() );
  VERIFY( *i2 == *val );

  auto(std::move(i2));
  i2 = make();
  VERIFY( *i2 == *val );
  VERIFY( src->valueless_after_move() );

  const Indirect val1(std::in_place, {1, 2, 3});
  const Indirect val2(std::in_place, {2, 4, 6});

  Indirect s1(val1);
  Indirect s2(val2);
  s1.swap(s2);
  VERIFY( *s2 == *val1 );
  VERIFY( *s1 == *val2 );

  auto(std::move(s1));

  s1.swap(s2);
  VERIFY( *s1 == *val1 );
  VERIFY( s2.valueless_after_move() );

  auto e = [] {
    Indirect res;
    auto(std::move(res));
    return res;
  };

  Indirect e1(e());
  VERIFY( e1.valueless_after_move() );

  Indirect e2(std::allocator_arg, {}, e());
  VERIFY( e2.valueless_after_move() );

  Indirect e3(val);
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
