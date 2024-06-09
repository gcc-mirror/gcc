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

void
test_explicit_object_call_operator()
{
  // LWG 3617 - function/packaged_task deduction guides and deducing this
  struct F {
    int operator()(this const F&) { return 42; }
  };

  std::packaged_task g{ F{} };
  check_type<std::packaged_task<int()>>(g);

  struct F2 {
    short operator()(this F2&, float&) { return 0; }
  };

  std::packaged_task g2{ F2{} };
  check_type<std::packaged_task<short(float&)>>(g2);

  struct F3 {
    void operator()(this const F3, char*, long) { }
  };

  std::packaged_task g3{ F3{} };
  check_type<std::packaged_task<void(char*, long)>>(g3);

  struct F4 {
    int i;
    operator const int&() const { return i; }
    const long& operator()(this int, const long& l) { return l; }
  };

  std::packaged_task g4{ F4{} };
  check_type<std::packaged_task<const long&(const long&)>>(g4);
}
