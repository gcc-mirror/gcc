// { dg-do run { target c++17 } }

// PR libstdc++/114401 allocator destructor omitted when reinserting node_handle

#include <set>
#include <memory>
#include <testsuite_hooks.h>

template<typename T>
struct Alloc
{
  using value_type = T;
  using propagate_on_container_copy_assignment = std::true_type;
  using propagate_on_container_move_assignment = std::true_type;
  using propagate_on_container_swap = std::true_type;

  Alloc(int identity) : id(std::make_shared<int>(identity)) { }

  template<typename U>
    Alloc(const Alloc<U> a) : id(a.id) { }

  T* allocate(std::size_t n) { return std::allocator<T>().allocate(n); }
  void deallocate(T* p, std::size_t n) { std::allocator<T>().deallocate(p, n); }

  template<typename U>
    friend bool
    operator==(const Alloc& a, const Alloc<U>& a2)
    { return a.id == a2.id; }

  template<typename U>
    friend bool
    operator!=(const Alloc& a, const Alloc<U>& a2)
    { return !(a == a2); }

  std::shared_ptr<int> id;
};

using test_type = std::multiset<int, std::less<int>, Alloc<int>>;

void
test_node_ops()
{
  test_type s1({1,3,5}, Alloc<int>(1));
  test_type s2({2,4,6,8}, Alloc<int>(2));
  VERIFY( s1.get_allocator() != s2.get_allocator() );

  auto node_a = s1.extract(1);
  VERIFY( ! node_a.empty() );
  VERIFY( node_a.get_allocator() == s1.get_allocator() );

  node_a = std::move(node_a); // self-move
  VERIFY( node_a.empty() );

  swap(node_a, node_a); // self-swap
  VERIFY( node_a.empty() );

  auto node_b = s2.extract(2);
  VERIFY( node_b.get_allocator() == s2.get_allocator() );

  node_a = std::move(node_b); // empty = !empty
  VERIFY( node_a.get_allocator() == s2.get_allocator() );
  VERIFY( node_b.empty() );

  swap(node_a, node_b); // swap(!empty, empty)
  VERIFY( node_a.empty() );
  VERIFY( node_b.get_allocator() == s2.get_allocator() );

  swap(node_a, node_b); // swap(empty, !empty)
  VERIFY( node_a.get_allocator() == s2.get_allocator() );
  VERIFY( node_b.empty() );

  node_a = s1.extract(3); // !empty = !empty
  VERIFY( node_a.get_allocator() == s1.get_allocator() );
  node_b = s2.extract(0); // empty = empty
  VERIFY( node_b.empty() );
  node_b = s2.extract(6); // empty = !empty
  VERIFY( node_b.get_allocator() == s2.get_allocator() );

  swap(node_a, node_b); // swap(!empty, !empty)
  VERIFY( node_a.get_allocator() == s2.get_allocator() );
  VERIFY( node_b.get_allocator() == s1.get_allocator() );

  node_a = {};
  node_b = std::move(node_a); // !empty = empty
  VERIFY( node_a.empty() );
  VERIFY( node_b.empty() );

  swap(node_a, node_b); // swap(empty, empty)
  VERIFY( node_a.empty() );
  VERIFY( node_b.empty() );
}

void
test_alloc_lifetime()
{
  Alloc<int> a(1);
  test_type s({1,2,3}, a);
  VERIFY( a.id.use_count() == 2 ); // a and the copy in s

  s.insert(s.extract(1));
  VERIFY( a.id.use_count() == 2 );

  s.insert(s.begin(), s.extract(2));
  VERIFY( a.id.use_count() == 2 );

  auto node = s.extract(1);
  VERIFY( a.id.use_count() == 3 );
  node = s.extract(0);
  VERIFY( a.id.use_count() == 2 );

  s.insert(std::move(node));
  VERIFY( a.id.use_count() == 2 );

  s.merge(test_type(s));
  VERIFY( a.id.use_count() == 2 );

  s.merge(test_type({4,5,6}, a));
  VERIFY( a.id.use_count() == 2 );
}

int main()
{
  test_node_ops();
  test_alloc_lifetime();
}
