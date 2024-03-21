// { dg-do run { target c++26 } }

#include <memory>
#include <scoped_allocator>
#include <utility>
#include <vector>

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using __gnu_test::tracker_allocator;
using Counter = __gnu_test::tracker_allocator_counter;
using Vector = std::vector<int>;
using Indirect = std::indirect<Vector, tracker_allocator<Vector>>;
const Indirect src(std::in_place, {1, 2, 3});

constexpr void
test_ctor()
{
  Counter::reset();
  Indirect i1(src);
  VERIFY( *i1 == *src );
  VERIFY( &*i1 != &*src );
  VERIFY( Counter::get_allocation_count() == sizeof(Vector) );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 1 );
  VERIFY( Counter::get_destruct_count() == 0 );

  Counter::reset();
  Indirect i2(std::allocator_arg, {}, src);
  VERIFY( *i2 == *src );
  VERIFY( &*i2 != &*src );
  VERIFY( Counter::get_allocation_count() == sizeof(Vector) );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 1 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

constexpr void
test_assign()
{
  Indirect i1;
  Counter::reset();

  i1 = src;
  VERIFY( *i1 == *src );
  VERIFY( &*i1 != &*src );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );

  auto(std::move(i1));
  Counter::reset();

  i1 = src;
  VERIFY( *i1 == *src );
  VERIFY( &*i1 != &*src );
  VERIFY( Counter::get_allocation_count() == sizeof(Vector) );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 1 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

constexpr void
test_valueless()
{
  Indirect e;
  auto(std::move(e));
  VERIFY( e.valueless_after_move() );

  Counter::reset();
  Indirect i1(e);
  VERIFY( i1.valueless_after_move() );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );

  Indirect i2(std::allocator_arg, {}, e);
  VERIFY( i2.valueless_after_move() );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );

  Indirect i3(src);
  Counter::reset();
  i3 = e;
  VERIFY( i3.valueless_after_move() );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == sizeof(Vector) );
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
