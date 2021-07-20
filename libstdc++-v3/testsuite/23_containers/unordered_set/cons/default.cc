// { dg-do compile { target c++11 } }
#include <unordered_set>

static_assert( std::is_default_constructible<std::unordered_set<int>>{}, "" );

template<typename T>
  struct NoDefaultConsAlloc
  {
    using value_type = T;

    NoDefaultConsAlloc(int) noexcept { }

    template<typename U>
      NoDefaultConsAlloc(const NoDefaultConsAlloc<U>&) { }

    T *allocate(std::size_t n)
    { return std::allocator<T>().allocate(n); }

    void deallocate(T *p, std::size_t n)
    { std::allocator<T>().deallocate(p, n); }
  };

using Set = std::unordered_set<int, std::hash<int>, std::equal_to<int>,
			       NoDefaultConsAlloc<int>>;
static_assert( ! std::is_default_constructible<Set>{}, "PR libstdc++/100863" );

struct Hash : std::hash<int> { Hash(int) { } };
using Set2 = std::unordered_set<int, Hash>;
static_assert( ! std::is_default_constructible<Set2>{}, "PR libstdc++/100863" );

struct Equal : std::equal_to<int> { Equal(int) { } };
using Set3 = std::unordered_set<int, std::hash<int>, Equal>;
static_assert( ! std::is_default_constructible<Set3>{}, "PR libstdc++/100863" );
