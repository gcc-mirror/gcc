// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <future>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test_static_call_operator()
{
  struct F1 { static long operator()() { return 0; } };
  std::packaged_task task1{ F1{} };
  check_type<std::packaged_task<long()>>(task1);

  struct F2 { static float operator()(char, void*) noexcept { return 0; } };
  std::packaged_task task2{ F2{} };
  check_type<std::packaged_task<float(char, void*)>>(task2);
}
