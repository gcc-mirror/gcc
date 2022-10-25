// { dg-do run { target c++11 } }
#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

template<typename T>
struct Alloc
{
  using value_type = T;
  explicit Alloc(int) { }
  template<typename U> Alloc(const Alloc&) { }

  T* allocate(std::size_t n)
  { return std::allocator<T>().allocate(n); }
  void deallocate(T* ptr, std::size_t n)
  { std::allocator<T>().deallocate(ptr, n); }

  bool operator==(const Alloc&) const { return true; }
  bool operator!=(const Alloc&) const { return false; }
};

void
test_non_default_constructible()
{
  using sub_match = std::sub_match<const char*>;
  using alloc_type = Alloc<sub_match>;
  using match_results = std::match_results<const char*, alloc_type>;
  match_results res(alloc_type(1));

  std::regex_match("x", res, std::regex(".")); // PR libstdc++/107376
}

template<typename T>
struct PropAlloc
{
  int id;

  using value_type = T;
  explicit PropAlloc(int id) : id(id) { }
  template<typename U> PropAlloc(const PropAlloc& a) : id(a.id) { }

  using propagate_on_container_move_assignment = std::true_type;
  using propagate_on_container_copy_assignment = std::true_type;

  PropAlloc select_on_container_copy_construction() const
  { return PropAlloc(0); }

  T* allocate(std::size_t n)
  { return std::allocator<T>().allocate(n); }
  void deallocate(T* ptr, std::size_t n)
  { std::allocator<T>().deallocate(ptr, n); }

  bool operator==(const PropAlloc& a) const { return id == a.id; }
  bool operator!=(const PropAlloc& a) const { return id != a.id; }
};

void
test_propagation()
{
  using sub_match = std::sub_match<const char*>;
  using alloc_type = PropAlloc<sub_match>;
  using match_results = std::match_results<const char*, alloc_type>;
  alloc_type alloc(107376);
  match_results res(alloc);

  std::regex re("..", std::regex_constants::__polynomial);
  std::regex_match("xx", res, re);

  VERIFY( res.get_allocator() == alloc );
}

int main()
{
  test_non_default_constructible();
  test_propagation();
}
