// { dg-do run { target c++26 } }

#include <memory>
#include <scoped_allocator>
#include <utility>
#include <vector>
#include <optional>

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using __gnu_test::propagating_allocator;
using __gnu_test::tracker_allocator;
using Counter = __gnu_test::tracker_allocator_counter;

constexpr void
verifyNoAllocations()
{
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

template<bool Propagate>
constexpr void
test_ctor()
{
  using PropAlloc = propagating_allocator<int, Propagate>;
  using Vector = std::vector<int, PropAlloc>;
  using ScopedAlloc = std::scoped_allocator_adaptor<
    propagating_allocator<Vector, Propagate, tracker_allocator<Vector>>,
    PropAlloc>;
  using Indirect = std::indirect<Vector, ScopedAlloc>;

  const Indirect val(std::in_place, {1, 2, 3});
  std::optional<Indirect> src;
  auto make = [&val, &src] -> Indirect&& {
    src.emplace(std::allocator_arg, ScopedAlloc{11, 22}, val);
    Counter::reset();
    return std::move(*src);
  };

  Indirect i1(make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );
  VERIFY( i1->get_allocator().get_personality() == 22 );
  VERIFY( i1.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  Indirect i2(std::allocator_arg, ScopedAlloc{11, 22}, make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i2 == *val );
  VERIFY( i2->get_allocator().get_personality() == 22 );
  VERIFY( i2.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  Indirect i3(std::allocator_arg, ScopedAlloc{33, 44}, make());
  // We move-from contained object
  VERIFY( !src->valueless_after_move() );
  VERIFY( *i3 == *val );
  VERIFY( i3->get_allocator().get_personality() == 44 );
  VERIFY( i3.get_allocator().get_personality() == 33 );
  VERIFY( Counter::get_allocation_count() == sizeof(Vector) );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 1 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

template<bool Propagate>
constexpr void
test_assign()
{
  using PropAlloc = propagating_allocator<int, Propagate>;
  using Vector = std::vector<int, PropAlloc>;
  using ScopedAlloc = std::scoped_allocator_adaptor<
    propagating_allocator<Vector, Propagate, tracker_allocator<Vector>>,
    PropAlloc>;
  using Indirect = std::indirect<Vector, ScopedAlloc>;

  const Indirect val(std::in_place, {1, 2, 3});
  std::optional<Indirect> src;
  auto make = [&val, &src] -> Indirect&& {
    src.emplace(std::allocator_arg, ScopedAlloc{11, 22}, val);
    Counter::reset();
    return std::move(*src);
  };

  Indirect i1(std::allocator_arg, ScopedAlloc{11, 22});

  i1 = make();
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );
  VERIFY( i1->get_allocator().get_personality() == 22 );
  VERIFY( i1.get_allocator().get_personality() == 11 );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == sizeof(Vector) );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 1 );

  Indirect i2(std::allocator_arg, ScopedAlloc{33, 44});

  i2 = make();
  VERIFY( *i2 == *val );
  if (Propagate)
  {
    VERIFY( src->valueless_after_move() );
    VERIFY( i2->get_allocator().get_personality() == 22 );
    VERIFY( i2.get_allocator().get_personality() == 11 );
    VERIFY( Counter::get_allocation_count() == 0 );
    VERIFY( Counter::get_construct_count() == 0 );
  }
  else
  {
    // We allocate new holder and move-from contained object
    VERIFY( !src->valueless_after_move() );
    VERIFY( i2->get_allocator().get_personality() == 44 );
    VERIFY( i2.get_allocator().get_personality() == 33 );
    VERIFY( Counter::get_allocation_count() == sizeof(Vector) );
    VERIFY( Counter::get_construct_count() == 1 );
  }
  VERIFY( Counter::get_deallocation_count() == sizeof(Vector) );
  VERIFY( Counter::get_destruct_count() == 1 );

  Indirect i3(std::allocator_arg, ScopedAlloc{11, 22});
  auto(std::move(i3));

  i3 = make();
  VERIFY( *i3 == *val );
  VERIFY( src->valueless_after_move() );
  VERIFY( i3->get_allocator().get_personality() == 22 );
  VERIFY( i3.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  Indirect i4(std::allocator_arg, ScopedAlloc{33, 44});
  auto(std::move(i4));

  i4 = make();
  VERIFY( *i4 == *val );
  if (Propagate)
  {
    VERIFY( src->valueless_after_move() );
    VERIFY( i4->get_allocator().get_personality() == 22 );
    VERIFY( i4.get_allocator().get_personality() == 11 );
    VERIFY( Counter::get_allocation_count() == 0 );
    VERIFY( Counter::get_construct_count() == 0 );
  }
  else
  {
    // We allocate new holder and move-from contained object
    VERIFY( !src->valueless_after_move() );
    VERIFY( i4->get_allocator().get_personality() == 44 );
    VERIFY( i4.get_allocator().get_personality() == 33 );
    VERIFY( Counter::get_allocation_count() == sizeof(Vector) );
    VERIFY( Counter::get_construct_count() == 1 );
  }
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

template<bool Propagate>
constexpr void
test_swap()
{
  using PropAlloc = propagating_allocator<int, Propagate>;
  using Vector = std::vector<int, PropAlloc>;
  using ScopedAlloc = std::scoped_allocator_adaptor<
    propagating_allocator<Vector, Propagate, tracker_allocator<Vector>>,
    PropAlloc>;
  using Indirect = std::indirect<Vector, ScopedAlloc>;

  const Indirect val1(std::in_place, {1, 2, 3});
  const Indirect val2(std::in_place, {2, 4, 6});

  Indirect i1(std::allocator_arg, ScopedAlloc{11, 22}, val1);
  Indirect i2(std::allocator_arg, ScopedAlloc{11, 22}, val2);
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

  if (!Propagate)
    return;

  Indirect i3(std::allocator_arg, ScopedAlloc{33, 44}, val2);
  Counter::reset();
  i1.swap(i3);
  VERIFY( *i1 == *val2 );
  VERIFY( i1->get_allocator().get_personality() == 44 );
  VERIFY( i1.get_allocator().get_personality() == 33 );
  VERIFY( *i3 == *val1 );
  VERIFY( i3->get_allocator().get_personality() == 22 );
  VERIFY( i3.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  i1.swap(i2);
  VERIFY( i1.valueless_after_move() );
  VERIFY( i1.get_allocator().get_personality() == 11 );
  VERIFY( *i2 == *val2 );
  VERIFY( i2->get_allocator().get_personality() == 44 );
  VERIFY( i2.get_allocator().get_personality() == 33 );
  verifyNoAllocations();
}

template<bool Propagate>
constexpr void
test_valueless()
{
  using PropAlloc = propagating_allocator<int, Propagate>;
  using Vector = std::vector<int, PropAlloc>;
  using ScopedAlloc = std::scoped_allocator_adaptor<
    propagating_allocator<Vector, Propagate, tracker_allocator<Vector>>,
    PropAlloc>;
  using Indirect = std::indirect<Vector, ScopedAlloc>;

  auto e = [] {
    Indirect res(std::allocator_arg, ScopedAlloc{11, 22});
    auto(std::move(res));
    Counter::reset();
    return res;
  };

  Indirect i1(e());
  VERIFY( i1.valueless_after_move() );
  VERIFY( i1.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  Indirect i2(std::allocator_arg, ScopedAlloc{33, 44}, e());
  VERIFY( i2.valueless_after_move() );
  VERIFY( i2.get_allocator().get_personality() == 33 );
  verifyNoAllocations();

  Indirect i3(std::allocator_arg, ScopedAlloc{33, 44});

  i3 = e();
  VERIFY( i3.valueless_after_move() );
  if (Propagate)
    VERIFY( i3.get_allocator().get_personality() == 11 );
  else
    VERIFY( i3.get_allocator().get_personality() == 33 );
  VERIFY( Counter::get_allocation_count() ==  0 );
  VERIFY( Counter::get_deallocation_count() == sizeof(Vector) );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 1 );

  i2 = e();
  VERIFY( i2.valueless_after_move() );
  if (Propagate)
    VERIFY( i2.get_allocator().get_personality() == 11 );
  else
    VERIFY( i2.get_allocator().get_personality() == 33 );
  verifyNoAllocations();

  i3.swap(i2);
  VERIFY( i2.valueless_after_move() );
  VERIFY( i1.valueless_after_move() );
  verifyNoAllocations();

  if (!Propagate)
    return;

  Indirect i4(std::allocator_arg, ScopedAlloc{33, 44}, e());
  i4.swap(i1);
  verifyNoAllocations();
}

template<bool Propagate>
constexpr void
test_all()
{
  test_ctor<Propagate>();
  test_assign<Propagate>();
  test_swap<Propagate>();
  test_valueless<Propagate>();
}

int main()
{
  test_all<true>();
  test_all<false>();

  static_assert([] {
    test_all<true>();
    test_all<false>();
    return true;
  });
}
