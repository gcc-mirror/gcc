// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-xfail-if "not supported" { debug_mode } }

#include <vector>
#include <testsuite_hooks.h>

template<typename T>
struct Alloc : std::allocator<T>
{
  using std::allocator<T>::allocator;

  constexpr explicit Alloc(int p) : personality(p) { }

  template<typename U>
    constexpr Alloc(const Alloc<U>& a) : personality(a.personality) { }

  int personality = 0;

  using propagate_on_container_move_assignment = std::false_type;

  constexpr bool operator==(const Alloc& a) const noexcept
  { return personality == a.personality; }
};

constexpr std::size_t
capacity_for(std::size_t n)
{
  std::size_t N = std::vector<bool>(1).capacity();
  if (auto r = n % N)
    return n - r + N;
  return n;
}

constexpr bool
copy_assign()
{
  // vector::operator=(const vector&)

  std::vector<bool> v1, v2;
  v1 = v1;
  v2 = v1;
  VERIFY(v2.size() == 0);
  VERIFY(v2.capacity() == 0);

  for (int i = 0; i < 10; ++i)
    v1.push_back(i);
  v2 = v1;
  VERIFY(v2.size() == 10);
  v2.reserve(50);
  v1.push_back(1);
  v2 = v1;
  VERIFY(v2.size() == 11);
  VERIFY(v2.capacity() == capacity_for(50));

  std::vector<bool, Alloc<bool>> va1(Alloc<bool>(1)), va2(Alloc<bool>(2));
  va2 = va1;
  VERIFY( va2.get_allocator().personality == 2 );
  va1.push_back(999);
  va2 = va1;
  VERIFY( va2.get_allocator().personality == 2 );

  return true;
}

static_assert( copy_assign() );

constexpr bool
move_assign()
{
  // vector::operator=(const vector&)

  std::vector<bool> v1, v2;
  v1 = std::move(v1);
  v2 = std::move(v1);
  VERIFY(v2.size() == 0);
  VERIFY(v2.capacity() == 0);

  for (int i = 0; i < 10; ++i)
    v1.push_back(i);
  v2 = std::move(v1);
  VERIFY(v2.size() == 10);
  v2.reserve(50);
  v1.push_back(1);
  v2 = std::move(v1);
  VERIFY(v2.size() == 1);
  VERIFY(v1.capacity() == 0);
  VERIFY(v2.capacity() == capacity_for(1));

  std::vector<bool, Alloc<bool>> va1(Alloc<bool>(1)), va2(Alloc<bool>(2));
  va2 = std::move(va1);
  VERIFY( va2.get_allocator().personality == 2 );
  va1.push_back(9);
  va1.push_back(99);
  va1.push_back(999);
  va1.push_back(9999);
  va2 = std::move(va1);
  va2 = std::move(va1);
  VERIFY( va2.get_allocator().personality == 2 );

  return true;
}

static_assert( move_assign() );

constexpr bool
initializer_list_assign()
{
  std::vector<bool> v1;
  v1 = {1, 0, 1};
  VERIFY( v1.size() == 3 );
  VERIFY( v1.capacity() == capacity_for(3) );
  v1 = {1, 0};
  VERIFY( v1.size() == 2 );
  VERIFY( v1.capacity() == capacity_for(3) );
  v1 = {1, 1, 0, 1, 1, 1, 0, 1, 1};
  VERIFY( v1.size() == 9 );
  VERIFY( v1.capacity() == capacity_for(9) );

  std::vector<bool, Alloc<bool>> va1(Alloc<bool>(111));
  va1 = {1, 0, 0};
  VERIFY( va1.get_allocator().personality == 111 );

  return true;
}

static_assert( initializer_list_assign() );

constexpr bool
assign_iterator_range()
{
  std::vector<bool> v;

  int arr[] = { 1, 2, 3, 4 };
  v.assign(arr, arr+3);
  VERIFY( v.size() == 3 );
  v.reserve(5);
  v.assign(arr, arr+3);
  VERIFY( v.capacity() == capacity_for(5) );

  struct input_iterator
  {
    using iterator_category = std::input_iterator_tag;
    using value_type = bool;
    using pointer = const bool*;
    using reference = bool;
    using difference_type = int;

    constexpr input_iterator() : val(0) { }
    constexpr input_iterator(int i) : val(i) { }

    constexpr input_iterator& operator++() { --val; return *this; }
    constexpr input_iterator operator++(int) { return {val--}; }

    constexpr bool operator*() const { return val % 2; }
    constexpr const bool* operator->() const { return nullptr; }

    constexpr bool operator==(const input_iterator&) const = default;

    int val;
  };

  v.assign(input_iterator(9), input_iterator());
  VERIFY( v.size() == 9 );

  return true;
}

static_assert( assign_iterator_range() );

constexpr bool
assign_value()
{
  std::vector<bool> v1;
  v1.assign(3, 8);
  VERIFY( v1.size() == 3 );
  VERIFY( v1.capacity() == capacity_for(3) );
  v1.assign(2, 9);
  VERIFY( v1.size() == 2 );
  VERIFY( v1.capacity() == capacity_for(3) );
  v1.assign(9, 10);
  VERIFY( v1.size() == 9 );
  VERIFY( v1.capacity() == capacity_for(9) );

  std::vector<bool, Alloc<bool>> va1(Alloc<bool>(111));
  va1.assign(2, 9);
  VERIFY( va1.size() == 2 );
  VERIFY( va1.get_allocator().personality == 111 );

  return true;
}

static_assert( assign_value() );

constexpr bool
assign_initializer_list()
{
  std::vector<bool> v1;
  v1.assign({0, 1, 0});
  VERIFY( v1.size() == 3 );
  VERIFY( v1.capacity() == capacity_for(3) );
  v1.assign({1, 0});
  VERIFY( v1.size() == 2 );
  VERIFY( v1.capacity() == capacity_for(3) );
  v1.assign({1, 0, 0, 1, 1, 0, 0, 1, 1});
  VERIFY( v1.size() == 9 );
  VERIFY( v1.capacity() == capacity_for(9) );

  std::vector<bool, Alloc<bool>> va1(Alloc<bool>(111));
  va1.assign({1, 1, 1});
  VERIFY( va1.get_allocator().personality == 111 );

  return true;
}

static_assert( assign_initializer_list() );
