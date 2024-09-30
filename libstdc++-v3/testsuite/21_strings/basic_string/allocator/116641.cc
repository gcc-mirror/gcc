// { dg-do run { target c++11 } }
// { dg-require-effective-target cxx11_abi }

// Bug 116641 - std::string move assignment incorrectly depends on POCCA

#include <string>
#include <testsuite_hooks.h>

template<typename T>
struct Alloc
{
  using value_type = T;
  using propagate_on_container_swap = std::false_type;
  using propagate_on_container_copy_assignment = std::true_type;
  using propagate_on_container_move_assignment = std::false_type;

  Alloc(int id) : id(id) { }

  template<typename U>
    Alloc(const Alloc<U>& a) : id(a.id) { }

  T* allocate(unsigned long n)
  { return std::allocator<T>().allocate(n); }

  void deallocate(T* p, unsigned long n)
  { std::allocator<T>().deallocate(p, n); }

  Alloc& operator=(const Alloc&) { throw; }

  bool operator==(const Alloc& a) const { return id == a.id; }
  bool operator!=(const Alloc& a) const { return id != a.id; }

  int id;
};

void
test_pr116641()
{
  Alloc<char> a1(1), a2(2);
  std::basic_string<char, std::char_traits<char>, Alloc<char>> s1(a1), s2(a2);

  s1 = "allocator should not propagate on move assignment";
  VERIFY( s1.get_allocator() == a1 );
  VERIFY( s2.get_allocator() == a2 );
  s2 = std::move(s1);
  VERIFY( s1.get_allocator() == a1 );
  VERIFY( s2.get_allocator() == a2 );
}

int main()
{
  test_pr116641();
}
