// { dg-do compile { target c++20 } }

#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

template<typename T>
struct Alloc : std::allocator<T>
{
  using std::allocator<T>::allocator;

  constexpr explicit Alloc(int p) : personality(p) { }

  template<typename U>
    constexpr Alloc(const Alloc<U>& a) : personality(a.personality) { }

#if __cplusplus <= 202302L
  using is_always_equal = std::false_type;
#endif

  int personality = 0;

  constexpr Alloc select_on_container_copy_construction() const
  { return Alloc(-1); }

  constexpr bool operator==(const Alloc& a) const noexcept
  { return personality == a.personality; }
};

namespace default_constructor_global_scope
{
  constexpr std::vector<bool> v1;
  static_assert(v1.size() == 0);
  static_assert(v1.capacity() == 0);

  constexpr std::allocator<bool> a;
  constexpr std::vector<bool> v2(a);
  static_assert(v2.size() == 0);
  static_assert(v2.capacity() == 0);

  constexpr Alloc<bool> aa(10);
  constexpr std::vector<bool, Alloc<bool>> v3(aa);
  static_assert(v3.size() == 0);
  static_assert(v3.capacity() == 0);
  static_assert(v3.get_allocator() == aa);
}

constexpr bool
default_constructor_function_scope()
{
  // vector()

  std::vector<bool> v1;
  VERIFY(v1.size() == 0);
  VERIFY(v1.capacity() == 0);

  // vector(const Allocator&)

  const std::allocator<bool> a;
  std::vector<bool> v2(a);
  VERIFY(v2.size() == 0);
  VERIFY(v2.capacity() == 0);

  const Alloc<bool> aa(10);
  std::vector<bool, Alloc<bool>> v3(aa);
  VERIFY(v3.size() == 0);
  VERIFY(v3.capacity() == 0);
  VERIFY(v3.get_allocator() == aa);

  return true;
}

static_assert( default_constructor_function_scope() );

constexpr bool
sequence_constructors()
{
  // vector(size_type, const Allocator& = Allocator())

  std::vector<bool> v0(0);
  VERIFY(v0.size() == 0);

  std::vector<bool> v1(1);
  VERIFY(v1.size() == 1);

  std::vector<bool> v2(2);
  VERIFY(v2.size() == 2);

  std::vector<bool> v50(50);
  VERIFY(v50.size() == 50);

  const std::allocator<bool> a;
  std::vector<bool> a0(0, a);
  VERIFY(a0.size() == 0);

  std::vector<bool> a1(1, a);
  VERIFY(a1.size() == 1);

  std::vector<bool> a2(2, a);
  VERIFY(a2.size() == 2);

  std::vector<bool> a50(50, a);
  VERIFY(a50.size() == 50);

  const Alloc<bool> la(10);
  std::vector<bool, Alloc<bool>> l0(0, la);
  VERIFY(l0.size() == 0);
  VERIFY(l0.get_allocator() == la);

  std::vector<bool, Alloc<bool>> l1(1, la);
  VERIFY(l1.size() == 1);
  VERIFY(l1.get_allocator() == la);

  std::vector<bool, Alloc<bool>> l2(2, la);
  VERIFY(l2.size() == 2);
  VERIFY(l2.get_allocator() == la);

  std::vector<bool, Alloc<bool>> l50(50, la);
  VERIFY(l50.size() == 50);
  VERIFY(l50.get_allocator() == la);

  // vector(size_type, const T&, const Allocator& = Allocator())

  std::vector<bool> v3(3, true);
  VERIFY(v3.size() == 3);
  VERIFY(v3[0] == true && v3[2] == true);

  std::vector<bool> a3(3, false, a);
  VERIFY(a3.size() == 3);
  VERIFY(a3[0] == false && a3[2] == false);

  std::vector<bool, Alloc<bool>> l3(3, true, la);
  VERIFY(l3.size() == 3);
  VERIFY(l3[0] == true && l3[2] == true);
  VERIFY(l3.get_allocator() == la);

  return true;
}

static_assert(sequence_constructors());

constexpr bool
iterator_range_constructor()
{
  // vector(InputIterator, InputIterator, const Allocator& = Allocator())

  short range[3] = { true, false, true };

  std::vector<bool> v0(std::begin(range), std::end(range));
  VERIFY(v0.size() == std::size(range));
  VERIFY(v0[0] == true && v0[1] == false && v0[2] == true);

  const Alloc<bool> a(5);
  std::vector<bool, Alloc<bool>> l0(std::begin(range), std::end(range), a);
  VERIFY(l0.size() == std::size(range));
  VERIFY(l0.get_allocator() == a);
  VERIFY(l0[0] == true && l0[1] == false && l0[2] == true);

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

  std::vector<bool> v1(input_iterator(3), input_iterator());
  VERIFY(v1.size() == 3);
  VERIFY(v1[0] == true && v1[1] == false && v1[2] == true);

  std::vector<bool, Alloc<bool>> l1(input_iterator(2), input_iterator(), a);
  VERIFY(l1.size() == 2);
  VERIFY(l1.get_allocator() == a);
  VERIFY(l1[0] == false && l1[1] == true);

  return true;
}

static_assert(iterator_range_constructor());

constexpr bool
initializer_list_constructor()
{
  // vector(initializer_list<T>, const Allocator& = Allocator())

  std::vector<bool> v0({ false, true, false });
  VERIFY(v0.size() == 3);
  VERIFY(v0[0] == false && v0[1] == true && v0[2] == false);

  const Alloc<bool> a(5);
  std::vector<bool, Alloc<bool>> l0({ true, false, false }, a);
  VERIFY(l0.size() == 3);
  VERIFY(l0.get_allocator() == a);
  VERIFY(l0[0] == true && l0[1] == false && l0[2] == false);

  return true;
}

static_assert(initializer_list_constructor());

constexpr bool
copy_constructor()
{
  const std::vector<bool> v0({ 1, 0, 0, 1, 0, 1, 1, 0 });
  const std::vector<bool, Alloc<bool>> l0({ 0, 0, 1, 1, 0, 1, 0, 1, 1, 1 });

  // vector(const vector&)

  std::vector<bool> v1(v0);
  VERIFY( v1.size() == v0.size() );
  VERIFY( v1[0] == v0[0] && v1[1] == v0[1] && v1[2] == v0[2] );
  VERIFY( v1.get_allocator() == v0.get_allocator() );

  const Alloc<bool> as(6);
  std::vector<bool, Alloc<bool>> s1(3, true, as);
  std::vector<bool, Alloc<bool>> s2(s1);
  VERIFY( s2.size() == s1.size() );
  VERIFY( s2.get_allocator().personality == -1 );

  // vector(const vector&, const Allocator&)

  const Alloc<bool> a(6);
  std::vector<bool, Alloc<bool>> l1(l0, a);
  VERIFY( l1.size() == l0.size() );
  VERIFY( l1[0] == l0[0] && l1[1] == l0[1] && l1[2] == l0[2] );
  VERIFY( l1.get_allocator() == a );
  VERIFY( l1.get_allocator() != l0.get_allocator() );

  return true;
}

static_assert(copy_constructor());

constexpr bool
move_constructor()
{
  const std::vector<bool> v0({ 1, 0, 0, 1, 0, 1, 1, 0 });
  const std::vector<bool, Alloc<bool>> l0({ 0, 0, 1, 1, 0, 1, 0, 1, 1, 1 });

  // vector(const vector&)

  std::vector<bool> v1(v0);
  std::vector<bool> v2(std::move(v1));
  VERIFY( v2.size() == v0.size() );
  VERIFY( v1.empty() );
  VERIFY( v2[0] == v0[0] && v2[1] == v0[1] && v2[2] == v0[2] );
  VERIFY( v2.get_allocator() == v0.get_allocator() );

  // vector(const vector&, const Allocator&)

  const Alloc<bool> a(6);
  std::vector<bool, Alloc<bool>> l1(l0);
  std::vector<bool, Alloc<bool>> l2(std::move(l1), a);
  VERIFY( l2.size() == l0.size() );
  VERIFY( l2[0] == l0[0] && l2[1] == l0[1] && l2[2] == l0[2] );
  VERIFY( l2.get_allocator() == a );
  VERIFY( l2.get_allocator() != l0.get_allocator() );

  std::vector<bool, Alloc<bool>> l3(std::move(l2), a);
  VERIFY( l3.size() == l0.size() );
  VERIFY( l3[0] == l0[0] && l3[1] == l0[1] && l3[2] == l0[2] );
  VERIFY( l3.get_allocator() == a );
  VERIFY( l3.get_allocator() == l2.get_allocator() );

  return true;
}

static_assert(move_constructor());
