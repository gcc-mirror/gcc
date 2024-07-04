// { dg-do compile { target c++23 } }
// { dg-require-effective-target hosted }

#include <functional>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test_static_call_operator()
{
  struct F1 { static long operator()() { return 0; } };
  std::function func1 = F1{};
  check_type<std::function<long()>>(func1);

  struct F2 { static float operator()(char, void*) noexcept { return 0; } };
  std::function func2 = F2{};
  check_type<std::function<float(char, void*)>>(func2);
}

void
test_explicit_object_call_operator()
{
  // LWG 3617 - function/packaged_task deduction guides and deducing this
  struct F {
    int operator()(this const F&) { return 42; }
  };

  std::function g = F{};
  check_type<std::function<int()>>(g);

  struct F2 {
    short operator()(this F2&, float&) { return 0; }
  };

  std::function g2 = F2{};
  check_type<std::function<short(float&)>>(g2);

  struct F3 {
    void operator()(this const F3, char*, long) { }
  };

  std::function g3 = F3{};
  check_type<std::function<void(char*, long)>>(g3);

  struct F4 {
    int i;
    operator const int&() const { return i; }
    const long& operator()(this int, const long& l) { return l; }
  };

  std::function g4 = F4{};
  check_type<std::function<const long&(const long&)>>(g4);
}
