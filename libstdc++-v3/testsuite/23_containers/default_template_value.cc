// { dg-do compile { target c++26 } }

#include <deque>
#include <forward_list>
#include <list>
#include <string>
#include <vector>

#if !defined(__cpp_lib_algorithm_default_value_type)
#error "Feature test macro for default template type for algorithms' values is missing"
#elif __cpp_lib_algorithm_default_value_type < 202403L
#error "Feature test macro for default template type for algorithms' values is wrong"
#endif

struct S {
  S(int, double);
  friend auto operator<=>(const S&, const S&) = default;
};

template<template<typename...> typename Container>
void test_erase()
{
  Container<S> c;
  std::erase(c, {1, 3.14});
}

void
test()
{
  test_erase<std::deque>();
  test_erase<std::forward_list>();
  test_erase<std::list>();
  test_erase<std::vector>();

  std::string s;
  std::erase(s, {'x'});

  std::wstring ws;
  std::erase(ws, {L'x'});
}
