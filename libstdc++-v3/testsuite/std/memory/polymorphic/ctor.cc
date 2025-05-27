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

using __gnu_test::uneq_allocator;
using UneqAlloc = uneq_allocator<int>;
using ScopedAlloc = std::scoped_allocator_adaptor<
  uneq_allocator<std::vector<int, UneqAlloc>>,
  UneqAlloc>;

struct Obj
{
  int i;
  char c[2];
};

constexpr void
test_default_ctor()
{
  using __gnu_test::default_init_allocator;

  std::polymorphic<Obj, default_init_allocator<Obj>> i1;
  default_init_allocator<int> a{};

  // The contained object and the allocator should be value-initialized.
  VERIFY( i1->i == 0 );
  VERIFY( i1->c[0] == 0 );
  VERIFY( i1->c[1] == 0 );
  VERIFY( i1.get_allocator() == a );

  a.state = 5;
  // Allocator-extended default constructor:
  std::polymorphic<Obj, default_init_allocator<Obj>> i2(std::allocator_arg, a);
  VERIFY( i2.get_allocator() == a );

  // Object is constructed using allocator-aware constructor.
  std::polymorphic<std::vector<int, UneqAlloc>, ScopedAlloc>
    i3(std::allocator_arg, ScopedAlloc(11, 22));
  VERIFY( i3->empty() );
  VERIFY( i3->get_allocator().get_personality() == 22 );
  VERIFY( i3.get_allocator().get_personality() == 11 );
}

constexpr void
test_forwarding_ctor()
{
  Obj obj{1, {'2', '3'}};
  auto verify = [](std::polymorphic<Obj> const& i)
  {
    VERIFY( i->i == 1 );
    VERIFY( i->c[0] == '2' );
    VERIFY( i->c[1] == '3' );
  };

  std::polymorphic<Obj> i1(std::as_const(obj));
  verify(i1);
  std::polymorphic<Obj> i2(std::move(std::as_const(obj)));
  verify(i2);
  std::polymorphic<Obj> i3(obj);
  verify(i3);
  std::polymorphic<Obj> i4(std::move(obj));
  verify(i4);

  std::polymorphic<Obj> i5({1, {'2', '3'}});
  verify(i5);

  std::vector<int, UneqAlloc> v{1, 2, 3, 4, 5};
  // Object is constructed using allocator-aware constructor.
  std::polymorphic<std::vector<int, UneqAlloc>, ScopedAlloc>
    i7(std::allocator_arg, ScopedAlloc(11, 22), v);
  VERIFY( i7->size() == 5  );
  VERIFY( v.size() == 5 );
  VERIFY( i7->get_allocator().get_personality() == 22 );
  VERIFY( i7.get_allocator().get_personality() == 11 );

  std::polymorphic<std::vector<int, UneqAlloc>, ScopedAlloc>
    i8(std::allocator_arg, ScopedAlloc(11, 22), std::move(v));
  VERIFY( i8->size() == 5  );
  VERIFY( v.size() == 0 );
  VERIFY( i8->get_allocator().get_personality() == 22 );
  VERIFY( i8.get_allocator().get_personality() == 11 );
}

constexpr void
test_inplace_ctor()
{
  std::polymorphic<Obj> i1(std::in_place_type<Obj>);
  VERIFY( i1->i == 0 );
  VERIFY( i1->c[0] == 0 );
  VERIFY( i1->c[1] == 0 );

  std::polymorphic<Obj> i2(std::in_place_type<Obj>, 10);
  VERIFY( i2->i == 10 );
  VERIFY( i2->c[0] == 0 );
  VERIFY( i2->c[1] == 0 );

  std::polymorphic<Obj, uneq_allocator<Obj>>
    i3(std::allocator_arg, 42, std::in_place_type<Obj>);
  VERIFY( i3->i == 0 );
  VERIFY( i3->c[0] == 0 );
  VERIFY( i3->c[1] == 0 );
  VERIFY( i3.get_allocator().get_personality() == 42 );

  std::polymorphic<Obj,  uneq_allocator<Obj>>
    i4(std::allocator_arg, 42, std::in_place_type<Obj>, 10);
  VERIFY( i4->i == 10 );
  VERIFY( i4->c[0] == 0 );
  VERIFY( i4->c[1] == 0 );
  VERIFY( i4.get_allocator().get_personality() == 42 );

  std::polymorphic<std::vector<int>>
    i5(std::in_place_type<std::vector<int>>);
  VERIFY( i5->size() == 0 );

  std::polymorphic<std::vector<int>>
    i6(std::in_place_type<std::vector<int>>, 5, 13);
  VERIFY( i6->size() == 5 );
  VERIFY( i6->at(0) == 13 );

  std::polymorphic<std::vector<int>>
    i7(std::in_place_type<std::vector<int>>, {1, 2, 3, 4});
  VERIFY( i7->size() == 4 );
  VERIFY( i7->at(2) == 3 );

  std::polymorphic<std::vector<int, UneqAlloc>>
    i8(std::in_place_type<std::vector<int, UneqAlloc>>, UneqAlloc{42});
  VERIFY( i8->size() == 0 );
  VERIFY( i8->get_allocator().get_personality() == 42 );

  std::polymorphic<std::vector<int, UneqAlloc>>
    i9(std::in_place_type<std::vector<int, UneqAlloc>>, 5, 13, UneqAlloc{42});
  VERIFY( i9->size() == 5 );
  VERIFY( i9->at(0) == 13 );
  VERIFY( i9->get_allocator().get_personality() == 42 );

  std::polymorphic<std::vector<int, UneqAlloc>>
    i10(std::in_place_type<std::vector<int, UneqAlloc>>, {1, 2, 3, 4}, UneqAlloc{42});
  VERIFY( i10->size() == 4 );
  VERIFY( i10->at(2) == 3 );
  VERIFY( i10->get_allocator().get_personality() == 42 );

  std::polymorphic<std::vector<int, UneqAlloc>, ScopedAlloc>
    i14(std::allocator_arg, ScopedAlloc(11, 22),
	std::in_place_type<std::vector<int, UneqAlloc>>);
  VERIFY( i14->size() == 0 );
  VERIFY( i14->get_allocator().get_personality() == 22 );
  VERIFY( i14.get_allocator().get_personality() == 11 );

  std::polymorphic<std::vector<int, UneqAlloc>, ScopedAlloc>
    i15(std::allocator_arg, ScopedAlloc(11, 22),
	std::in_place_type<std::vector<int, UneqAlloc>>, 5, 13);
  VERIFY( i15->size() == 5 );
  VERIFY( i15->at(0) == 13 );
  VERIFY( i15->get_allocator().get_personality() == 22 );
  VERIFY( i15.get_allocator().get_personality() == 11 );

  std::polymorphic<std::vector<int, UneqAlloc>, ScopedAlloc>
    i16(std::allocator_arg, ScopedAlloc(11, 22),
	std::in_place_type<std::vector<int, UneqAlloc>>, {1, 2, 3, 4});
  VERIFY( i16->size() == 4 );
  VERIFY( i16->at(2) == 3 );
  VERIFY( i16->get_allocator().get_personality() == 22 );
  VERIFY( i16.get_allocator().get_personality() == 11 );
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
