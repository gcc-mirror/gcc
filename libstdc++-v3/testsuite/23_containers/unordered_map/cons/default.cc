// { dg-do compile { target c++11 } }
#include <unordered_map>

static_assert( std::is_default_constructible<std::unordered_map<int, int>>{}, "" );

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

using Map = std::unordered_map<int, int, std::hash<int>, std::equal_to<int>,
			       NoDefaultConsAlloc<std::pair<const int, int>>>;
static_assert( ! std::is_default_constructible<Map>{}, "PR libstdc++/100863" );

struct Hash : std::hash<int> { Hash(int) { } };
using Map2 = std::unordered_map<int, int, Hash>;
static_assert( ! std::is_default_constructible<Map2>{}, "PR libstdc++/100863" );

struct Equal : std::equal_to<int> { Equal(int) { } };
using Map3 = std::unordered_map<int, int, std::hash<int>, Equal>;
static_assert( ! std::is_default_constructible<Map3>{}, "PR libstdc++/100863" );
