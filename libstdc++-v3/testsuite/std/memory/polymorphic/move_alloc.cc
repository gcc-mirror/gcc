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

  virtual constexpr int
  get_alloc_personality() const
  { return -1; }

private:
  constexpr virtual bool
  eq(const Base& other) const = 0;
};

template<typename T, typename Allocator>
struct VecDerived : Base, std::vector<T, Allocator>
{
  using VecBase = std::vector<T, Allocator>;

  using VecBase::VecBase;

  constexpr int
  get_alloc_personality() const override
  { return this->get_allocator().get_personality(); }

private:

  constexpr bool
  eq(const Base& other) const override
  { 
    if (auto op = dynamic_cast<const VecDerived*>(&other))
      return *static_cast<const VecBase*>(this)
	       == *static_cast<const VecBase*>(op);
    return false;
  }
};

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
  using Vector = VecDerived<int, PropAlloc>;
  using ScopedAlloc = std::scoped_allocator_adaptor<
    propagating_allocator<Vector, Propagate, tracker_allocator<Vector>>,
    PropAlloc>;
  using Polymorphic = std::polymorphic<Vector, ScopedAlloc>;

  const Polymorphic val(std::in_place_type<Vector>, {1, 2, 3});
  std::optional<Polymorphic> src;
  auto make = [&val, &src] -> Polymorphic&& {
    src.emplace(std::allocator_arg, ScopedAlloc{11, 22}, val);
    Counter::reset();
    return std::move(*src);
  };

  Polymorphic i1(make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );
  VERIFY( i1->get_alloc_personality() == 22 );
  VERIFY( i1.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  Polymorphic i2(std::allocator_arg, ScopedAlloc{11, 22}, make());
  VERIFY( src->valueless_after_move() );
  VERIFY( *i2 == *val );
  VERIFY( i2->get_alloc_personality() == 22 );
  VERIFY( i2.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  Polymorphic i3(std::allocator_arg, ScopedAlloc{33, 44}, make());
  // We move-from contained object
  VERIFY( !src->valueless_after_move() );
  VERIFY( *i3 == *val );
  VERIFY( i3->get_alloc_personality() == 44 );
  VERIFY( i3.get_allocator().get_personality() == 33 );
  VERIFY( Counter::get_allocation_count() >= sizeof(Vector) );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

template<bool Propagate>
constexpr void
test_assign()
{
  using PropAlloc = propagating_allocator<int, Propagate>;
  using Vector = VecDerived<int, PropAlloc>;
  using ScopedAlloc = std::scoped_allocator_adaptor<
    propagating_allocator<Vector, Propagate, tracker_allocator<Vector>>,
    PropAlloc>;
  using Polymorphic = std::polymorphic<Vector, ScopedAlloc>;

  const Polymorphic val(std::in_place_type<Vector>, {1, 2, 3});
  std::optional<Polymorphic> src;
  auto make = [&val, &src] -> Polymorphic&& {
    src.emplace(std::allocator_arg, ScopedAlloc{11, 22}, val);
    Counter::reset();
    return std::move(*src);
  };

  Counter::reset();
  Polymorphic i1(std::allocator_arg, ScopedAlloc{11, 22});
  const std::size_t holderSize = Counter::get_allocation_count();
  VERIFY( holderSize >= sizeof(Vector) );

  i1 = make();
  VERIFY( src->valueless_after_move() );
  VERIFY( *i1 == *val );
  VERIFY( i1->get_alloc_personality() == 22 );
  VERIFY( i1.get_allocator().get_personality() == 11 );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == holderSize );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 1 );

  Polymorphic i2(std::allocator_arg, ScopedAlloc{33, 44});

  i2 = make();
  VERIFY( *i2 == *val );
  if (Propagate)
  {
    VERIFY( src->valueless_after_move() );
    VERIFY( i2->get_alloc_personality() == 22 );
    VERIFY( i2.get_allocator().get_personality() == 11 );
    VERIFY( Counter::get_allocation_count() == 0 );
    VERIFY( Counter::get_construct_count() == 0 );
  }
  else
  {
    // We allocate new holder and move-from contained object
    VERIFY( !src->valueless_after_move() );
    VERIFY( i2->get_alloc_personality() == 44 );
    VERIFY( i2.get_allocator().get_personality() == 33 );
    VERIFY( Counter::get_allocation_count() == holderSize );
    VERIFY( Counter::get_construct_count() == 2 );
  }
  VERIFY( Counter::get_deallocation_count() == holderSize );
  VERIFY( Counter::get_destruct_count() == 1 );

  Polymorphic i3(std::allocator_arg, ScopedAlloc{11, 22},
		 std::in_place_type<Vector>);
  auto(std::move(i3));

  i3 = make();
  VERIFY( *i3 == *val );
  VERIFY( src->valueless_after_move() );
  VERIFY( i3->get_alloc_personality() == 22 );
  VERIFY( i3.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  Polymorphic i4(std::allocator_arg, ScopedAlloc{33, 44},
		 std::in_place_type<Vector>);
  auto(std::move(i4));

  i4 = make();
  VERIFY( *i4 == *val );
  if (Propagate)
  {
    VERIFY( src->valueless_after_move() );
    VERIFY( i4->get_alloc_personality() == 22 );
    VERIFY( i4.get_allocator().get_personality() == 11 );
    VERIFY( Counter::get_allocation_count() == 0 );
    VERIFY( Counter::get_construct_count() == 0 );
  }
  else
  {
    // We allocate new holder and move-from contained object
    VERIFY( !src->valueless_after_move() );
    VERIFY( i4->get_alloc_personality() == 44 );
    VERIFY( i4.get_allocator().get_personality() == 33 );
    VERIFY( Counter::get_allocation_count() == holderSize );
    VERIFY( Counter::get_construct_count() == 2 );
  }
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

template<bool Propagate>
constexpr void
test_swap()
{
  using PropAlloc = propagating_allocator<int, Propagate>;
  using Vector = VecDerived<int, PropAlloc>;
  using ScopedAlloc = std::scoped_allocator_adaptor<
    propagating_allocator<Vector, Propagate, tracker_allocator<Vector>>,
    PropAlloc>;
  using Polymorphic = std::polymorphic<Vector, ScopedAlloc>;

  const Polymorphic val1(std::in_place_type<Vector>, {1, 2, 3});
  const Polymorphic val2(std::in_place_type<Vector>, {2, 4, 6});

  Polymorphic i1(std::allocator_arg, ScopedAlloc{11, 22}, val1);
  Polymorphic i2(std::allocator_arg, ScopedAlloc{11, 22}, val2);
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

  Polymorphic i3(std::allocator_arg, ScopedAlloc{33, 44}, val2);
  Counter::reset();
  i1.swap(i3);
  VERIFY( *i1 == *val2 );
  VERIFY( i1->get_alloc_personality() == 44 );
  VERIFY( i1.get_allocator().get_personality() == 33 );
  VERIFY( *i3 == *val1 );
  VERIFY( i3->get_alloc_personality() == 22 );
  VERIFY( i3.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  i1.swap(i2);
  VERIFY( i1.valueless_after_move() );
  VERIFY( i1.get_allocator().get_personality() == 11 );
  VERIFY( *i2 == *val2 );
  VERIFY( i2->get_alloc_personality() == 44 );
  VERIFY( i2.get_allocator().get_personality() == 33 );
  verifyNoAllocations();
}

template<bool Propagate>
constexpr void
test_valueless()
{
  using PropAlloc = propagating_allocator<int, Propagate>;
  using Vector = VecDerived<int, PropAlloc>;
  using ScopedAlloc = std::scoped_allocator_adaptor<
    propagating_allocator<Vector, Propagate, tracker_allocator<Vector>>,
    PropAlloc>;
  using Polymorphic = std::polymorphic<Vector, ScopedAlloc>;

  auto e = [] {
    Polymorphic res(std::allocator_arg, ScopedAlloc{11, 22},
		    std::in_place_type<Vector>);
    auto(std::move(res));
    Counter::reset();
    return res;
  };

  Polymorphic i1(e());
  VERIFY( i1.valueless_after_move() );
  VERIFY( i1.get_allocator().get_personality() == 11 );
  verifyNoAllocations();

  Polymorphic i2(std::allocator_arg, ScopedAlloc{33, 44}, e());
  VERIFY( i2.valueless_after_move() );
  VERIFY( i2.get_allocator().get_personality() == 33 );
  verifyNoAllocations();

  Polymorphic i3(std::allocator_arg, ScopedAlloc{33, 44});

  i3 = e();
  VERIFY( i3.valueless_after_move() );
  if (Propagate)
    VERIFY( i3.get_allocator().get_personality() == 11 );
  else
    VERIFY( i3.get_allocator().get_personality() == 33 );
  VERIFY( Counter::get_allocation_count() ==  0 );
  VERIFY( Counter::get_deallocation_count() >= sizeof(Vector) );
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

  Polymorphic i4(std::allocator_arg, ScopedAlloc{33, 44}, e());
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
