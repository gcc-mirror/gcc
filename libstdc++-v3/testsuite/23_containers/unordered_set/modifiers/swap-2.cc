// { dg-do run { target c++11 } }

// Bug 117921 - containers do not use ADL swap for Compare, Pred or Hash types

#include <unordered_set>
#include <testsuite_hooks.h>

namespace adl
{
  struct Hash : std::hash<int>
  {
    static bool swapped;
    friend void swap(Hash&, Hash&) { swapped = true; }
  };
  bool Hash::swapped = false;

  struct Eq : std::equal_to<int>
  {
    static bool swapped;
    friend void swap(Eq&, Eq&) { swapped = true; }
  };
  bool Eq::swapped = false;

  struct Allocator_base
  {
    static bool swapped;
  };
  bool Allocator_base::swapped = false;

  using std::size_t;

  template<typename T>
    struct Allocator : Allocator_base
    {
      using value_type = T;

      Allocator() { }
      template<typename U> Allocator(const Allocator<U>&) { }

      T* allocate(size_t n) { return std::allocator<T>().allocate(n); }
      void deallocate(T* p, size_t n) { std::allocator<T>().deallocate(p, n); }

      using propagate_on_container_swap = std::true_type;

      friend void swap(Allocator&, Allocator&) { swapped = true; }
      friend bool operator==(Allocator, Allocator) { return true; }
    };
}

void
test_swap()
{
  std::unordered_set<int, adl::Eq, adl::Hash, adl::Allocator<int>> s1, s2;
  s1.swap(s2);
  VERIFY( adl::Hash::swapped );
  VERIFY( adl::Eq::swapped );
  VERIFY( adl::Allocator_base::swapped );
}

int main()
{
  test_swap();
}
