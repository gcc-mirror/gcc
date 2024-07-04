// { dg-do compile { target c++20 } }

#include <vector>

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
  using test_type = std::vector<int, Alloc<int>>;
  std::pair<const int, wrap<test_type>>* h = nullptr;
  do_adl(h);
}

void test_pr113841_bool()
{
  using test_type = std::vector<bool, Alloc<bool>>;
  std::pair<const int, wrap<test_type>>* h = nullptr;
  do_adl(h);
}
