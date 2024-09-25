// { dg-do compile { target c++20 } }

#include <string>

template<typename T>
struct Alloc
{
  using value_type = T;

  Alloc(int) { } // not default constructible

  template<typename U> Alloc(const Alloc<U>&) { }

  T* allocate(std::size_t n) { return std::allocator<T>().allocate(n); }
  void deallocate(T* p, std::size_t n) { std::allocator<T>().deallocate(p, n); }
};

template<typename T> struct wrap { T t; };

template<typename T> void do_adl(T&) { }

void test_pr113841()
{
  using Tr = std::char_traits<char>;
  using test_type = std::basic_string<char, Tr, Alloc<char>>;
  std::pair<const int, wrap<test_type>>* h = nullptr;
  do_adl(h);
}
