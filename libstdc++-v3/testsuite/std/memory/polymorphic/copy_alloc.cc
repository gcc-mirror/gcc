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

  const Polymorphic src(std::allocator_arg, ScopedAlloc{11, 22},
		     std::in_place_type<Vector>, {1, 2, 3});

  Counter::reset();
  Polymorphic i1(src);
  VERIFY( *i1 == *src );
  VERIFY( &*i1 != &*src );
  if (Propagate)
  {
    VERIFY( i1->get_alloc_personality() == 22 );
    VERIFY( i1.get_allocator().get_personality() == 11 );
  }
  else
  {
    VERIFY( i1->get_alloc_personality() == 0 );
    VERIFY( i1.get_allocator().get_personality() == 0 );
  }
  VERIFY( Counter::get_allocation_count() >= sizeof(Vector) );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 0 );


  Counter::reset();
  Polymorphic i2(std::allocator_arg, ScopedAlloc{33, 44}, src);
  VERIFY( *i2 == *src );
  VERIFY( &*i2 != &*src );
  VERIFY( i2->get_alloc_personality() == 44 );
  VERIFY( i2.get_allocator().get_personality() == 33 );
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

  const Polymorphic src(std::allocator_arg, ScopedAlloc{11, 22},
		     std::in_place_type<Vector>, {1, 2, 3});

  Counter::reset();
  Polymorphic i1(std::allocator_arg, ScopedAlloc{11, 22},
	         std::in_place_type<Vector>);
  const size_t holderSize = Counter::get_allocation_count();
  VERIFY( holderSize >= sizeof(Vector) );
  Counter::reset();

  i1 = src;
  VERIFY( *i1 == *src );
  VERIFY( &*i1 != &*src );
  VERIFY( i1->get_alloc_personality() == 22 );
  VERIFY( i1.get_allocator().get_personality() == 11 );
  VERIFY( Counter::get_allocation_count() == holderSize );
  VERIFY( Counter::get_deallocation_count() == holderSize );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 1 );

  Polymorphic i2(std::allocator_arg, ScopedAlloc{33, 44});
  Counter::reset();

  i2 = src;
  VERIFY( *i2 == *src );
  VERIFY( &*i2 != &*src );
  if (Propagate)
  {
    VERIFY( i2->get_alloc_personality() == 22 );
    VERIFY( i2.get_allocator().get_personality() == 11 );
  }
  else
  {
    VERIFY( i2->get_alloc_personality() == 44 );
    VERIFY( i2.get_allocator().get_personality() == 33 );
  }
  VERIFY( Counter::get_allocation_count() == holderSize );
  VERIFY( Counter::get_deallocation_count() == holderSize );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 1 );

  Polymorphic i3(std::allocator_arg, ScopedAlloc{11, 22});
  auto(std::move(i3));
  Counter::reset();

  i3 = src;
  VERIFY( *i3 == *src );
  VERIFY( &*i3 != &*src );
  VERIFY( i3->get_alloc_personality() == 22 );
  VERIFY( i3.get_allocator().get_personality() == 11 );
  VERIFY( Counter::get_allocation_count() == holderSize );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 0 );

  Polymorphic i4(std::allocator_arg, ScopedAlloc{33, 44});
  auto(std::move(i4));
  Counter::reset();

  i4 = src;
  VERIFY( *i4 == *src );
  VERIFY( &*i4 != &*src );
  if (Propagate)
  {
    VERIFY( i4->get_alloc_personality() == 22 );
    VERIFY( i4.get_allocator().get_personality() == 11 );
  }
  else
  {
    VERIFY( i4->get_alloc_personality() == 44 );
    VERIFY( i4.get_allocator().get_personality() == 33 );
  }
  VERIFY( Counter::get_allocation_count() == holderSize );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 2 );
  VERIFY( Counter::get_destruct_count() == 0 );
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

  Polymorphic e(std::allocator_arg, ScopedAlloc{11, 22},
	     std::in_place_type<Vector>);
  auto(std::move(e));
  VERIFY( e.valueless_after_move() );

  Counter::reset();
  Polymorphic i1(e);
  VERIFY( i1.valueless_after_move() );
  if (Propagate)
    VERIFY( i1.get_allocator().get_personality() == 11 );
  else
    VERIFY( i1.get_allocator().get_personality() == 0 );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );

  Counter::reset();
  Polymorphic i2(std::allocator_arg, ScopedAlloc{33, 44}, e);
  VERIFY( i2.valueless_after_move() );
  VERIFY( i2.get_allocator().get_personality() == 33 );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );

  Polymorphic i3(std::allocator_arg, ScopedAlloc{33, 44});
  Counter::reset();

  i3 = e;
  VERIFY( i3.valueless_after_move() );
  if (Propagate)
    VERIFY( i3.get_allocator().get_personality() == 11 );
  else
    VERIFY( i3.get_allocator().get_personality() == 33 );
  VERIFY( Counter::get_allocation_count() ==  0 );
  VERIFY( Counter::get_deallocation_count() >= sizeof(Vector) );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 1 );

  Counter::reset();
  i2 = e;
  VERIFY( i2.valueless_after_move() );
  if (Propagate)
    VERIFY( i2.get_allocator().get_personality() == 11 );
  else
    VERIFY( i2.get_allocator().get_personality() == 33 );
  VERIFY( Counter::get_allocation_count() == 0 );
  VERIFY( Counter::get_deallocation_count() == 0 );
  VERIFY( Counter::get_construct_count() == 0 );
  VERIFY( Counter::get_destruct_count() == 0 );
}

template<bool Propagate>
constexpr void
test_all()
{
  test_ctor<Propagate>();
  test_assign<Propagate>();
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
