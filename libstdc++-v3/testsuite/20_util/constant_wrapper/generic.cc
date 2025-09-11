// { dg-do run { target c++26 } }
#include <type_traits>
#include <utility>
#include <string_view>

#include <testsuite_hooks.h>

constexpr void
check_same(auto actual, auto expected)
{
  VERIFY(actual == expected);
  static_assert(std::same_as<decltype(actual), decltype(expected)>);
};


constexpr void
test_c_arrays()
{
  constexpr double x[] = {1.1, 2.2, 3.3};
  auto cx = std::cw<x>;
  auto access = [](auto x, size_t i)
  { return x[i]; };

  check_same(access(std::cw<x>, 0), x[0]);
  check_same(access(std::cw<x>, 1), x[1]);
  check_same(access(std::cw<x>, 2), x[2]);

  check_same(cx[std::cw<0>], std::cw<x[0]>);
  check_same(cx[std::cw<1>], std::cw<x[1]>);
  check_same(cx[std::cw<2>], std::cw<x[2]>);
}

constexpr size_t
deduce_cstr_size(auto str)
{
  size_t sz = 0;
  while(str[sz++] != '\0') { }
  return sz;
}

constexpr void
test_string_literals()
{
  auto foo = std::cw<"foo">;
  constexpr const typename decltype(foo)::value_type & cstr = foo;
  constexpr size_t N = std::size(cstr);
  constexpr auto foo_view = std::string_view(cstr, N-1);

  constexpr const char (&cstr1)[deduce_cstr_size(foo)] = foo;
  constexpr size_t N1 = std::size(cstr);

  static_assert(static_cast<char const*>(cstr) ==
		static_cast<char const*>(cstr1));
  static_assert(N1 == N);

  static_assert(foo[0] == 'f');
  static_assert(foo[1] == 'o');
  static_assert(foo[2] == 'o');
  static_assert(foo[3] == '\0');
  static_assert(static_cast<char const *>(foo) == foo_view);
}

constexpr bool
convert_constexpr(auto c)
{
  if constexpr (int(c) > 0)
    return true;
  return false;
}

constexpr void
test_converted_constexpr()
{
  VERIFY(!convert_constexpr(std::cw<-1>));
  VERIFY(convert_constexpr(std::cw<1>));
}

constexpr void
test_ints()
{
  std::constant_wrapper<2> two;
  std::constant_wrapper<3> three;
  std::constant_wrapper<5> five;

  VERIFY(two + 3 == 5);
  static_assert(std::same_as<decltype(two + 3), int>);

  VERIFY(two + three == 5);
  VERIFY(two + three == five);
  static_assert(std::same_as<decltype(two + three), std::constant_wrapper<5>>);

  VERIFY(two == std::cw<2>);
  VERIFY(two + 3 == std::cw<5>);
}

constexpr int
add(int i, int j)
{ return i + j; }

struct Add
{
  constexpr int
  operator()(int i, int j) const noexcept
  { return i + j; }
};

constexpr void
test_function_object()
{
  auto check = [](auto cfo)
  {
    auto ci = std::cw<2>;
    auto cj = std::cw<3>;

    VERIFY(cfo(ci, cj) == 5);
    static_assert(std::same_as<decltype(cfo(ci, cj)), std::constant_wrapper<5>>);

    static_assert(std::invocable<decltype(cfo), decltype(ci), decltype(cj)>);
    static_assert(!std::invocable<decltype(cfo), int, decltype(cj)>);
    static_assert(!std::invocable<decltype(cfo), int, int>);
  };

  check(std::cw<Add{}>);
  check(std::cw<[](int i, int j){ return i + j; }>);
  check(std::cw<[](auto i, auto j){ return i + j; }>);
}

constexpr void
test_function_pointer()
{
  auto cptr = std::cw<add>;
  auto ci = std::cw<2>;
  auto cj = std::cw<3>;

  VERIFY(cptr(ci, cj) == 5);
  static_assert(std::same_as<decltype(cptr(ci, cj)), std::constant_wrapper<5>>);

  VERIFY(cptr(2, cj) == 5);
  static_assert(std::same_as<decltype(cptr(2, cj)), int>);

  VERIFY(cptr(2, 3) == 5);
  static_assert(std::same_as<decltype(cptr(2, 3)), int>);
}

struct Indexable1
{
  constexpr int
  operator[](int i, int j) const noexcept
  { return i*j; }
};

template<typename Obj, typename... Args>
  concept indexable = requires (Obj obj, Args... args)
  {
    obj[args...];
  };

constexpr void
test_indexable1()
{
  auto cind = std::cw<Indexable1{}>;
  auto ci = std::cw<2>;
  auto cj = std::cw<3>;
  VERIFY(cind[ci, cj] == ci*cj);
  static_assert(std::same_as<decltype(cind[ci, cj]), std::constant_wrapper<6>>);

  static_assert(indexable<decltype(cind), decltype(ci), decltype(cj)>);
  static_assert(!indexable<decltype(cind), int, decltype(cj)>);
  static_assert(!indexable<decltype(cind), int, int>);
}

struct Indexable2
{
  template<typename I, typename J>
    constexpr int
    operator[](I i, J j) const noexcept
    { return i*j; }
};

constexpr void
test_indexable2()
{
  auto cind = std::cw<Indexable2{}>;
  auto ci = std::cw<2>;
  auto cj = std::cw<3>;
  VERIFY(cind[ci, cj] == ci*cj);
  static_assert(std::same_as<decltype(cind[ci, cj]), std::constant_wrapper<6>>);

  static_assert(indexable<decltype(cind), decltype(ci), decltype(cj)>);
  static_assert(!indexable<decltype(cind), int, decltype(cj)>);
  static_assert(!indexable<decltype(cind), int, int>);
}

struct Indexable3
{
  template<typename... Is>
    constexpr int
    operator[](Is... i) const noexcept
    { return (1 * ... * i); }
};

constexpr void
test_indexable3()
{
  auto cind = std::cw<Indexable3{}>;
  auto ci = std::cw<2>;
  auto cj = std::cw<3>;

  check_same(cind[], std::cw<1>);
  check_same(cind[ci], std::cw<2>);
  check_same(cind[ci, cj], std::cw<2*3>);
}

struct Divide
{
  int value;

  constexpr int
  divide(int div) const
  { return value / div; }

};

constexpr void
test_member_pointer()
{
  constexpr int nom = 42;
  constexpr int denom = 3;

  auto cvalue = std::cw<&Divide::value>;
  auto cdiv = std::cw<&Divide::divide>;
  auto co = std::cw<Divide{nom}>;

  check_same((&co)->*cvalue, std::cw<nom>);
  check_same((&co)->*(&Divide::value), nom);
  check_same(&(co.value)->*cvalue, nom);

  auto expect_unwrapped = nom / denom;
  check_same(((&co)->*(&Divide::divide))(denom), expect_unwrapped);
  check_same((&(co.value)->*cdiv)(denom), expect_unwrapped);
  check_same(((&decltype(co)::value)->*cdiv)(denom), expect_unwrapped);
}

constexpr void
test_pseudo_mutator()
{
  auto ci = std::cw<3>;
  auto cmmi = --ci;
  VERIFY(ci.value == 3);
  VERIFY(cmmi.value == 2);

  auto cimm = ci--;
  VERIFY(ci.value == 3);
  VERIFY(cimm.value == 3);
}

struct Truthy
{
  constexpr operator bool() const
  { return true; }
};

template<typename Lhs, typename Rhs>
  concept has_op_and = requires (Lhs lhs, Rhs rhs)
  {
    lhs && rhs;
  };

constexpr void
test_logic()
{
  auto ctrue = std::cw<true>;
  auto cfalse = std::cw<false>;
  auto truthy = Truthy{};

  auto check_and = [](auto lhs, auto rhs)
  {
    static_assert(lhs && rhs);
    static_assert(std::same_as<decltype(lhs && rhs), bool>);
  };

  auto check_or = [](auto lhs, auto rhs)
  {
    static_assert(lhs || rhs);
    static_assert(std::same_as<decltype(lhs || rhs), bool>);
  };

  check_and(ctrue, ctrue);
  check_or(ctrue, cfalse);
  check_and((std::cw<0> < std::cw<1>), (std::cw<1> < std::cw<5>));
  check_or((std::cw<0> < std::cw<1>), (std::cw<1> < std::cw<5>));

  auto ctruthy = std::cw<Truthy{}>;
  static_assert(has_op_and<decltype(truthy), bool>);
  static_assert(!has_op_and<decltype(ctruthy), decltype(ctrue)>);
  static_assert(!has_op_and<decltype(ctruthy), bool>);
}

struct ThreeWayComp
{
  friend
  constexpr std::strong_ordering
  operator<=>(ThreeWayComp lhs, ThreeWayComp rhs)
  { return lhs.value <=> rhs.value; }

  int value;
};

constexpr void
test_three_way()
{
  auto ctrue = std::cw<true>;
  auto cfalse = std::cw<false>;

  check_same(std::cw<ThreeWayComp{0}> < std::cw<ThreeWayComp{1}>, ctrue);
  check_same(std::cw<ThreeWayComp{2}> > std::cw<ThreeWayComp{1}>, ctrue);
  check_same(std::cw<ThreeWayComp{2}> >= std::cw<ThreeWayComp{1}>, ctrue);
  check_same(std::cw<ThreeWayComp{0}> <= std::cw<ThreeWayComp{1}>, ctrue);
  check_same(std::cw<ThreeWayComp{0}> >= std::cw<ThreeWayComp{1}>, cfalse);

  check_same(std::cw<ThreeWayComp{0}> < ThreeWayComp{1}, true);
  check_same(ThreeWayComp{2} > std::cw<ThreeWayComp{1}>, true);
}

struct EqualityComp
{
  friend
  constexpr bool
  operator==(EqualityComp lhs, EqualityComp rhs)
  { return lhs.value == rhs.value; }

  int value;
};

constexpr void
test_equality()
{
  auto ctrue = std::cw<true>;
  check_same(std::cw<EqualityComp{1}> == std::cw<EqualityComp{1}>, ctrue);
  check_same(std::cw<EqualityComp{0}> != std::cw<EqualityComp{1}>, ctrue);

  check_same(std::cw<EqualityComp{1}> == EqualityComp{1}, true);
  check_same(EqualityComp{0} != std::cw<EqualityComp{1}>, true);
}

struct ConstAssignable
{
  int value;

  constexpr ConstAssignable
  operator=(int rhs) const
  { return ConstAssignable{rhs}; }

  friend constexpr bool
  operator==(const ConstAssignable& lhs, const ConstAssignable& rhs)
  { return lhs.value == rhs.value; }
};

constexpr void
test_assignment()
{
  check_same(std::cw<ConstAssignable{3}> = std::cw<2>,
	     std::cw<ConstAssignable{2}>);
}


constexpr bool
test_all()
{
  test_c_arrays();
  test_ints();
  test_function_object();
  test_function_pointer();
  test_indexable1();
  test_indexable2();
  test_indexable3();
  test_member_pointer();
  test_pseudo_mutator();
  test_logic();
  test_three_way();
  test_equality();
  return true;
}

int
main()
{
  test_all();
  static_assert(test_all());
  return 0;
}
