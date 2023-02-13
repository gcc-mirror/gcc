// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <functional>

struct Foo
{
  void func() {}
};

void bar() { }

// PR libstdc++/107784 - QOI: sizeof( bind_front( Member-Function ) ) too big
static_assert( sizeof(std::bind_front(&Foo::func)) == sizeof(&Foo::func) );
static_assert( sizeof(std::bind_front(&bar)) == sizeof(&bar) );

// PR libstdc++/108290 - QoI: bind_front captureless lambda is too big
auto empty_lambda = [](auto, auto) { return 0; };

struct {
  void operator()(int, int, int) { }
  template<typename T> void operator()(T, T) { }
} empty_class;

static_assert(sizeof(std::bind_front(empty_lambda)) == 1);
static_assert(sizeof(std::bind_front(empty_lambda, 1)) == sizeof(int));
static_assert(sizeof(std::bind_front(empty_lambda, empty_lambda)) == 2);
static_assert(sizeof(std::bind_front(empty_lambda, empty_class)) == 1);
static_assert(sizeof(std::bind_front(empty_lambda, 1, 2)) == 2 * sizeof(int));
static_assert(sizeof(std::bind_front(empty_lambda, '1', empty_lambda)) == 2);
static_assert(sizeof(std::bind_front(empty_lambda, '1', empty_class)) == 1);

static_assert(sizeof(std::bind_front(empty_class)) == 1);
static_assert(sizeof(std::bind_front(empty_class, 1)) == sizeof(int));
static_assert(sizeof(std::bind_front(empty_class, empty_lambda)) == 1);
static_assert(sizeof(std::bind_front(empty_class, empty_class)) == 2);
static_assert(sizeof(std::bind_front(empty_class, 1, 2)) == 2 * sizeof(int));
static_assert(sizeof(std::bind_front(empty_class, '1', empty_lambda)) == 1);
static_assert(sizeof(std::bind_front(empty_class, '1', empty_class)) == 2);

struct derived1 : decltype(std::bind_front(empty_class))
{
  int i;
};
static_assert(sizeof(derived1) == sizeof(int));

struct derived2 : decltype(std::bind_front(empty_class, empty_lambda))
{
  int i;
};
static_assert(sizeof(derived2) == sizeof(int));
