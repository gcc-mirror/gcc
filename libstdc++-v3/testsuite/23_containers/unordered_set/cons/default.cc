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

    bool operator==(const NoDefaultConsAlloc&) const { return true; }
    bool operator!=(const NoDefaultConsAlloc&) const { return false; }
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

// PR libstdc++/101583
// verify non-default ctors can still be used
using Set4 = std::unordered_set<int, Hash, Equal, NoDefaultConsAlloc<int>>;
Hash h(1);
Equal eq(1);
Set4::allocator_type a(1);
Set4 s{1, h, eq, a};
Set4 s2{s.begin(), s.end(), s.size(), h, eq, a};
Set4 s3{{1, 2, 3}, 3, h, eq, a};
Set4 s4{s};
Set4 s5{s, a};
Set4 s6{std::move(s)};
Set4 s7{std::move(s6), a};

// PR libstdc++/126949
struct ExplicitHash
{
  explicit ExplicitHash(bool = false) { }

  std::size_t operator()(int value) const
  { return value; }
};

struct ExplicitEqual
{
  explicit ExplicitEqual(bool = false) { }

  bool operator()(int lhs, int rhs) const
  { return lhs == rhs; }
};

template<typename T>
  struct ExplicitAlloc
  {
    using value_type = T;

    explicit ExplicitAlloc(bool = false) noexcept { }

    template<typename U>
      ExplicitAlloc(const ExplicitAlloc<U>&) { }

    T *allocate(std::size_t n)
    { return std::allocator<T>().allocate(n); }

    void deallocate(T *p, std::size_t n)
    { std::allocator<T>().deallocate(p, n); }

    bool operator==(const ExplicitAlloc&) const { return true; }
    bool operator!=(const ExplicitAlloc&) const { return false; }
  };

void
test_explicit_hash()
{
  std::unordered_set<int, ExplicitHash> set1;
  std::unordered_set<int, std::hash<int>, ExplicitEqual> set2;
  std::unordered_set<int, std::hash<int>, std::equal_to<int>,
			  ExplicitAlloc<int>> set3;
  std::unordered_set<int, ExplicitHash, ExplicitEqual,
			  ExplicitAlloc<int>> set4;
}
