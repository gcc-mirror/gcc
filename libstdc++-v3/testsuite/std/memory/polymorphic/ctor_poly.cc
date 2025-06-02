// { dg-do run { target c++26 } }

#include <memory>
#include <scoped_allocator>
#include <utility>
#include <vector>

#ifndef __cpp_lib_polymorphic
# error __cpp_lib_polymorphic feature test macro missing in <memory>
#elif __cpp_lib_polymorphic != 202502
# error __cpp_lib_polymorphic feature test macro has wrong value in <memory>
#endif

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct Base {
  friend constexpr
  bool operator==(const Base& lhs, const Base& rhs)
  { return lhs.eq(rhs); }

  virtual constexpr int
  get_personality() const
  { return -1; }

private:
  constexpr virtual bool
  eq(const Base& other) const
  { return true; }
};

struct ObjDerived : Base
{
  constexpr ObjDerived()
   : x(0), y(0), z(0)
  { }

  constexpr ObjDerived(int a, int b, int c)
   : x(a), y(b), z(c)
  { }

  virtual constexpr int
  get_personality() const
  { return -2; }

private:
  constexpr bool
  eq(const Base& other) const override
  { 
    if (auto op = dynamic_cast<const ObjDerived*>(&other))
      return this->x == op->x && this->y == op->y && this->z == op->z;
    return false;
  }

  int x;
  int y;
  int z;
};

template<typename T, typename Allocator>
struct VecDerived : Base, std::vector<T, Allocator>
{
  using VecBase = std::vector<T, Allocator>;

  using VecBase::VecBase;

  constexpr int
  get_personality() const override
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

using __gnu_test::uneq_allocator;
using UneqAlloc = uneq_allocator<int>;
using ScopedAlloc = std::scoped_allocator_adaptor<
  uneq_allocator<Base>,
  UneqAlloc>;

constexpr void
test_default_ctor()
{
  using __gnu_test::default_init_allocator;

  std::polymorphic<Base, default_init_allocator<Base>> i1;
  default_init_allocator<int> a{};

  // The contained object and the allocator should be value-initialized.
  VERIFY( *i1 == Base() );
  VERIFY( i1->get_personality() == -1 );
  VERIFY( i1.get_allocator() == a );

  a.state = 5;
  // Allocator-extended default constructor:
  std::polymorphic<Base, default_init_allocator<Base>> i2(std::allocator_arg, a);
  VERIFY( *i1 == Base() );
  VERIFY( i1->get_personality() == -1 );
}

constexpr void
test_forwarding_ctor()
{
  const ObjDerived src(1, 2, 3);
 
  std::polymorphic<Base> i1(src);
  VERIFY( *i1 == src );
  VERIFY( i1->get_personality() == -2 );
  std::polymorphic<Base> i2(std::move(src));
  VERIFY( *i2 == src );
  VERIFY( i2->get_personality() == -2 );
  
  ObjDerived obj = src;
  std::polymorphic<Base> i3(obj);
  VERIFY( *i3 == src );
  VERIFY( i3->get_personality() == -2 );
  std::polymorphic<Base> i4(std::move(obj));
  VERIFY( *i4 == src );
  VERIFY( i4->get_personality() == -2 );

  const VecDerived<int, UneqAlloc> v{1, 2, 3, 4, 5};
  // Object is constructed using allocator-aware constructor.
  std::polymorphic<Base, ScopedAlloc>
    i5(std::allocator_arg, ScopedAlloc(11, 22), v);
  VERIFY( *i5 == v );
  VERIFY( i5->get_personality() == 22 );
  VERIFY( i5.get_allocator().get_personality() == 11 );

  std::polymorphic<Base, ScopedAlloc>
    i6(std::allocator_arg, ScopedAlloc(11, 22), auto(v));
  VERIFY( *i6 == v  );
  VERIFY( i6->get_personality() == 22 );
  VERIFY( i6.get_allocator().get_personality() == 11 );
}

constexpr void
test_inplace_ctor()
{
  std::polymorphic<Base> i1(std::in_place_type<ObjDerived>);
  VERIFY( *i1 == ObjDerived() );
  VERIFY( i1->get_personality() == -2 );

  std::polymorphic<Base> i2(std::in_place_type<ObjDerived>, 10, 20, 30);
  VERIFY( *i2 == ObjDerived(10, 20, 30) );
  VERIFY( i2->get_personality() == -2 );

  std::polymorphic<Base, uneq_allocator<Base>>
    i3(std::allocator_arg, 42, std::in_place_type<ObjDerived>);
  VERIFY( *i3 == ObjDerived() );
  VERIFY( i3->get_personality() == -2 );
  VERIFY( i3.get_allocator().get_personality() == 42 );

  std::polymorphic<Base, uneq_allocator<Base>>
    i4(std::allocator_arg, 42, std::in_place_type<ObjDerived>, 10, 20, 30);
  VERIFY( *i4 == ObjDerived(10, 20, 30) );
  VERIFY( i4->get_personality() == -2 );
  VERIFY( i4.get_allocator().get_personality() == 42 );

  const VecDerived<int, UneqAlloc> ze;
  const VecDerived<int, UneqAlloc> fe(5, 13);
  const VecDerived<int, UneqAlloc> il{1, 2, 3 ,4};

  std::polymorphic<Base>
    i5(std::in_place_type<VecDerived<int, UneqAlloc>>, UneqAlloc{42});
  VERIFY( *i5 == ze );
  VERIFY( i5->get_personality() == 42 );
 
  std::polymorphic<Base>
    i6(std::in_place_type<VecDerived<int, UneqAlloc>>, 5, 13, UneqAlloc{42});
  VERIFY( *i6 == fe );
  VERIFY( i6->get_personality() == 42 );

  std::polymorphic<Base>
    i7(std::in_place_type<VecDerived<int, UneqAlloc>>, {1, 2, 3, 4}, UneqAlloc{42});
  VERIFY( *i7 == il  );
  VERIFY( i7->get_personality() == 42 );

  std::polymorphic<Base, ScopedAlloc>
    i8(std::allocator_arg, ScopedAlloc(11, 22),
	std::in_place_type<VecDerived<int, UneqAlloc>>);
  VERIFY( *i8 == ze );
  VERIFY( i8->get_personality() == 22 );
  VERIFY( i8.get_allocator().get_personality() == 11 );

  std::polymorphic<Base, ScopedAlloc>
    i9(std::allocator_arg, ScopedAlloc(11, 22),
       std::in_place_type<VecDerived<int, UneqAlloc>>, 5, 13);
  VERIFY( *i9 == fe );
  VERIFY( i9->get_personality() == 22 );
  VERIFY( i9.get_allocator().get_personality() == 11 );

  std::polymorphic<Base, ScopedAlloc>
    i10(std::allocator_arg, ScopedAlloc(11, 22),
	std::in_place_type<VecDerived<int, UneqAlloc>>, {1, 2, 3, 4});
  VERIFY( *i10 == il );
  VERIFY( i10->get_personality() == 22 );
  VERIFY( i10.get_allocator().get_personality() == 11 );
}

int main()
{
  test_default_ctor();
  test_forwarding_ctor();
  test_inplace_ctor();

  static_assert([] {
    test_default_ctor();
    test_forwarding_ctor();
    test_inplace_ctor();
    return true;
  });
}
