// { dg-do compile { target c++26 } }

#include <span>
#include <type_traits>

#if !defined(__cpp_lib_span_initializer_list)
# error "__cpp_lib_span_initializer_list should be defined"
#elif __cpp_lib_span_initializer_list < 202311L
# error "Wrong value for __cpp_lib_span_initializer_list (should be >= 202311L)"
#endif

// Check the constraint on the initializer_list constructor
static_assert( std::is_const_v<std::span<const int>::element_type>);
static_assert(!std::is_const_v<std::span<      int>::element_type>);

static_assert( std::is_constructible_v<std::span<const int    >, std::initializer_list<      int>>);
static_assert( std::is_constructible_v<std::span<const int    >, std::initializer_list<const int>>);
static_assert( std::is_constructible_v<std::span<const int, 42>, std::initializer_list<      int>>);
static_assert( std::is_constructible_v<std::span<const int, 42>, std::initializer_list<const int>>);
static_assert(!std::is_constructible_v<std::span<      int    >, std::initializer_list<      int>>);
static_assert(!std::is_constructible_v<std::span<      int    >, std::initializer_list<const int>>);
static_assert(!std::is_constructible_v<std::span<      int, 42>, std::initializer_list<      int>>);
static_assert(!std::is_constructible_v<std::span<      int, 42>, std::initializer_list<const int>>);

// Check the explicit-ness on the initializer_list constructor
static_assert( std::is_convertible_v<std::initializer_list<      int>, std::span<const int    >>);
static_assert( std::is_convertible_v<std::initializer_list<const int>, std::span<const int    >>);
static_assert(!std::is_convertible_v<std::initializer_list<      int>, std::span<const int, 42>>);
static_assert(!std::is_convertible_v<std::initializer_list<const int>, std::span<const int, 42>>);
static_assert(!std::is_convertible_v<std::initializer_list<      int>, std::span<      int    >>);
static_assert(!std::is_convertible_v<std::initializer_list<const int>, std::span<      int    >>);
static_assert(!std::is_convertible_v<std::initializer_list<      int>, std::span<      int, 42>>);
static_assert(!std::is_convertible_v<std::initializer_list<const int>, std::span<      int, 42>>);

constexpr size_t fun1(std::span<const int> s)
{
  return s.size();
}

static_assert(fun1({}) == 0);
static_assert(fun1({1, 2, 3}) == 3);
static_assert(fun1(std::initializer_list<int>{1, 2, 3}) == 3);

// Stress-test array->pointer decays
struct decayer {
  constexpr decayer() = default;
  constexpr decayer(decayer *) {}
};

constexpr size_t fun2(std::span<const decayer> s)
{
  return s.size();
}

void test01()
{
  int intArray[42];
  static_assert(fun1(intArray) == 42);

  decayer decArray[42];
  static_assert(fun2(decArray) == 42);
  static_assert(fun2({decArray}) == 1); // decayer[] -> decayer* -> decayer(decayer*) -> init_list<decayer> of 1 element
  static_assert(fun2({decArray, decArray + 42}) == 2); // does not select span(iterator, iterator)
  static_assert(fun2({decArray, decArray, decArray}) == 3);
}
