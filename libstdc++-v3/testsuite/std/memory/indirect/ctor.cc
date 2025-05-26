// { dg-do run { target c++26 } }

#include <memory>
#include <scoped_allocator>
#include <utility>
#include <vector>

#ifndef __cpp_lib_indirect
# error __cpp_lib_indirect feature test macro missing in <memory>
#elif __cpp_lib_indirect != 202502
# error __cpp_lib_indirect feature test macro has wrong value in <memory>
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
test_deduction_guides()
{
  const Obj o{};
  std::indirect i1(o);
  static_assert(std::is_same_v<decltype(i1), std::indirect<Obj>>);

  using Alloc = __gnu_test::SimpleAllocator<Obj>;
  Alloc a;
  std::indirect i2(std::allocator_arg, a, o);
  static_assert(std::is_same_v<decltype(i2), std::indirect<Obj, Alloc>>);
}

constexpr void
test_default_ctor()
{
  using __gnu_test::default_init_allocator;

  std::indirect<Obj, default_init_allocator<Obj>> i1;
  default_init_allocator<int> a{};

  // The contained object and the allocator should be value-initialized.
  VERIFY( i1->i == 0 );
  VERIFY( i1->c[0] == 0 );
  VERIFY( i1->c[1] == 0 );
  VERIFY( i1.get_allocator() == a );

  a.state = 5;
  // Allocator-extended default constructor:
  std::indirect<Obj, default_init_allocator<Obj>> i2(std::allocator_arg, a);
  VERIFY( i2.get_allocator() == a );

  // Object is constructed using allocator-aware constructor.
  std::indirect<std::vector<int, UneqAlloc>, ScopedAlloc>
    i3(std::allocator_arg, ScopedAlloc(11, 22));
  VERIFY( i3->empty() );
  VERIFY( i3->get_allocator().get_personality() == 22 );
  VERIFY( i3.get_allocator().get_personality() == 11 );
}

constexpr void
test_forwarding_ctor()
{
  Obj obj{1, {'2', '3'}};
  auto verify = [](std::indirect<Obj> const& i)
  {
    VERIFY( i->i == 1 );
    VERIFY( i->c[0] == '2' );
    VERIFY( i->c[1] == '3' );
  };

  std::indirect<Obj> i1(std::as_const(obj));
  verify(i1);
  std::indirect<Obj> i2(std::move(std::as_const(obj)));
  verify(i2);
  std::indirect<Obj> i3(obj);
  verify(i3);
  std::indirect<Obj> i4(std::move(obj));
  verify(i4);

  std::indirect<Obj> i5({1, {'2', '3'}});
  verify(i5);

  // Aggregate parens init
  std::indirect<Obj> i6(7);
  VERIFY( i6->i == 7 );

  std::vector<int, UneqAlloc> v{1, 2, 3, 4, 5};
  // Object is constructed using allocator-aware constructor.
  std::indirect<std::vector<int, UneqAlloc>, ScopedAlloc>
    i7(std::allocator_arg, ScopedAlloc(11, 22), v);
  VERIFY( i7->size() == 5  );
  VERIFY( v.size() == 5 );
  VERIFY( i7->get_allocator().get_personality() == 22 );
  VERIFY( i7.get_allocator().get_personality() == 11 );

  std::indirect<std::vector<int, UneqAlloc>, ScopedAlloc>
    i8(std::allocator_arg, ScopedAlloc(11, 22), std::move(v));
  VERIFY( i8->size() == 5  );
  VERIFY( v.size() == 0 );
  VERIFY( i8->get_allocator().get_personality() == 22 );
  VERIFY( i8.get_allocator().get_personality() == 11 );
}

constexpr void
test_inplace_ctor()
{
  std::indirect<Obj> i1(std::in_place);
  VERIFY( i1->i == 0 );
  VERIFY( i1->c[0] == 0 );
  VERIFY( i1->c[1] == 0 );

  std::indirect<Obj> i2(std::in_place, 10);
  VERIFY( i2->i == 10 );
  VERIFY( i2->c[0] == 0 );
  VERIFY( i2->c[1] == 0 );

  std::indirect<Obj, uneq_allocator<Obj>>
    i3(std::allocator_arg, 42, std::in_place);
  VERIFY( i3->i == 0 );
  VERIFY( i3->c[0] == 0 );
  VERIFY( i3->c[1] == 0 );
  VERIFY( i3.get_allocator().get_personality() == 42 );

  std::indirect<Obj,  uneq_allocator<Obj>>
    i4(std::allocator_arg, 42, std::in_place, 10);
  VERIFY( i4->i == 10 );
  VERIFY( i4->c[0] == 0 );
  VERIFY( i4->c[1] == 0 );
  VERIFY( i4.get_allocator().get_personality() == 42 );

  std::indirect<std::vector<int>> i5(std::in_place);
  VERIFY( i5->size() == 0 );

  std::indirect<std::vector<int>> i6(std::in_place, 5, 13);
  VERIFY( i6->size() == 5 );
  VERIFY( i6->at(0) == 13 );

  std::indirect<std::vector<int>> i7(std::in_place, {1, 2, 3, 4});
  VERIFY( i7->size() == 4 );
  VERIFY( i7->at(2) == 3 );

  std::indirect<std::vector<int, UneqAlloc>>
    i8(std::in_place, UneqAlloc{42});
  VERIFY( i8->size() == 0 );
  VERIFY( i8->get_allocator().get_personality() == 42 );

  std::indirect<std::vector<int, UneqAlloc>>
    i9(std::in_place, 5, 13, UneqAlloc{42});
  VERIFY( i9->size() == 5 );
  VERIFY( i9->at(0) == 13 );
  VERIFY( i9->get_allocator().get_personality() == 42 );

  std::indirect<std::vector<int, UneqAlloc>>
    i10(std::in_place, {1, 2, 3, 4}, UneqAlloc{42});
  VERIFY( i10->size() == 4 );
  VERIFY( i10->at(2) == 3 );
  VERIFY( i10->get_allocator().get_personality() == 42 );

  std::indirect<std::vector<int, UneqAlloc>, ScopedAlloc>
    i14(std::allocator_arg, ScopedAlloc(11, 22),
	std::in_place);
  VERIFY( i14->size() == 0 );
  VERIFY( i14->get_allocator().get_personality() == 22 );
  VERIFY( i14.get_allocator().get_personality() == 11 );

  std::indirect<std::vector<int, UneqAlloc>, ScopedAlloc>
    i15(std::allocator_arg, ScopedAlloc(11, 22),
	std::in_place, 5, 13);
  VERIFY( i15->size() == 5 );
  VERIFY( i15->at(0) == 13 );
  VERIFY( i15->get_allocator().get_personality() == 22 );
  VERIFY( i15.get_allocator().get_personality() == 11 );

  std::indirect<std::vector<int, UneqAlloc>, ScopedAlloc>
    i16(std::allocator_arg, ScopedAlloc(11, 22),
	std::in_place, {1, 2, 3, 4});
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
