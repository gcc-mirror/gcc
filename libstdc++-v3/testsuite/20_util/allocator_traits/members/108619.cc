// { dg-do compile { target c++11 } }

#include <memory>

template<typename T>
struct Alloc
{
  Alloc() = default;

  template<typename U> Alloc(const Alloc<U>&) { }

  using value_type = T;

  T* allocate(unsigned n)
  { return std::allocator<T>().allocate(n); }

  void deallocate(T* p, unsigned n)
  { return std::allocator<T>().deallocate(p, n); }

  template<typename U> void destroy(U* p){ p->~U(); }
};


class S
{
  ~S() = default;

  friend Alloc<S>;
};

void
test_pr108619(Alloc<int> a, S* p)
{
  std::allocator_traits<Alloc<int>>::construct(a, p);
}
