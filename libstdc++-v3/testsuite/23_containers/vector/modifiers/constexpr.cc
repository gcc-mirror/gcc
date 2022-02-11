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

  constexpr bool operator==(const Alloc& a) const noexcept
  { return personality == a.personality; }
};

constexpr bool
test_push_back()
{
  std::vector<int> v;
  int& r = v.emplace_back(7);
  VERIFY( r == 7 );
  VERIFY( &r == &v.front() );
  v.emplace_back(r);
  v.emplace_back(v.front());
  v.emplace_back(v.back());
  VERIFY( v.size() == 4 );
  v.emplace_back(8);
  VERIFY( v.size() == 5 );
  VERIFY( v.back() == 8 );

  v.pop_back();
  VERIFY( v.size() == 4 );
  VERIFY( v.back() == 7 );
  v.pop_back();
  v.pop_back();
  v.pop_back();
  v.pop_back();
  VERIFY( v.empty() );

  v.push_back(99);
  for (std::size_t i = 0, c = v.capacity(); i <= c; ++i)
    v.push_back(v.front());
  VERIFY( v.capacity() > v.size() );

  std::vector<int, Alloc<int>> va;
  va.push_back(99);
  va.push_back(va.front());
  VERIFY( va.size() == 2 );

  return true;
}

static_assert( test_push_back() );

template<typename T = int>
constexpr std::false_type
pop_back_empty() { return {}; }

template<typename T = int>
requires (std::bool_constant<(std::vector<T>().pop_back(), true)>::value)
constexpr std::true_type
pop_back_empty() { return {}; }

static_assert( ! pop_back_empty() );

constexpr bool
test_insert_erase()
{
  std::vector<int> v;

  // vector::emplace(const_iterator, Args&&...)
  auto p = v.emplace(v.begin());
  VERIFY( p == v.begin() );
  p = v.emplace(v.end(), 7);
  VERIFY( p == --v.end() );

  // vector::insert(const_iterator, const T&)
  p = v.insert(v.begin(), *p);
  VERIFY( p == v.begin() );
  VERIFY( *p == 7 );
  VERIFY( &*p == &v.front() );
  // vector::insert(const_iterator, T&&)
  p = v.insert(v.end(), 1);
  VERIFY( p == --v.end() );
  v.insert(p, v.front());
  v.insert(v.end(), v.back());
  VERIFY( v.size() == 6 );
  v.insert(v.end(), 8);
  VERIFY( v.size() == 7 );
  VERIFY( v.back() == 8 );

  // vector::insert(const_iterator, size_type, const T&)
  v.insert(v.begin(), 2, v.front());
  v.insert(v.end(), 3, 99);
  VERIFY( v.size() == 12 );

  struct input_iterator
  {
    using iterator_category = std::input_iterator_tag;
    using value_type = int;
    using pointer = const int*;
    using reference = int;
    using difference_type = int;

    constexpr input_iterator() : val(0) { }
    constexpr input_iterator(int i) : val(i) { }

    constexpr input_iterator& operator++() { --val; return *this; }
    constexpr input_iterator operator++(int) { return {val--}; }

    constexpr int operator*() const { return val; }
    constexpr const int* operator->() const { return &val; }

    constexpr bool operator==(const input_iterator&) const = default;

    int val;
  };

  // vector::insert(const_iterator, Iter, Iter);
  v.insert(v.begin() + 2, input_iterator(), input_iterator());
  VERIFY( v.size() == 12 );
  v.reserve(13);
  auto n = v.capacity() - v.size();
  v.insert(v.end() - 9, input_iterator(n), input_iterator()); // no reallocation
  VERIFY( v.size() == (12 + n) );
  short a[] = { 84, 85 };
  v.insert(v.end() - 1, a, a + 2); // reallocation needed
  VERIFY( v.size() == (12 + n + 2) );
  v.resize(32);

  // vector::insert(const_iterator, initializer_list<T>)
  v.insert(v.begin(), {1,2,3});
  VERIFY( v.size() == 35 );

  v.rbegin()[0] = 999;
  v.rbegin()[1] = 888;

  // vector::erase(const_iterator)
  v.erase(v.end() - 1);
  VERIFY( v.size() == 34 );
  VERIFY( v.back() == 888 );
  v.erase(v.begin());
  v.erase(v.begin() + 1);
  v.erase(v.end() - 1);
  VERIFY( v.size() == 31 );

  // vector::erase(const_iterator, const_iterator)
  v.erase(v.begin(), v.begin());
  v.erase(v.end(), v.end());
  v.erase(v.begin(), v.begin() + 1);
  VERIFY( v.size() == 30 );
  v.erase(v.begin() + 2, v.end() - 2);
  VERIFY( v.size() == 4 );
  v.erase(v.begin(), v.end());
  VERIFY( v.empty() );
  v.erase( v.begin(), v.begin() );
  VERIFY( v.empty() );

  v.insert(v.end(), 99);
  for (std::size_t i = 0, c = v.capacity(); i <= c; ++i)
    v.insert(v.end() - 1, v.front());
  VERIFY( v.capacity() > v.size() );
  v.insert(v.end(), 999);
  for (std::size_t i = 0, c = v.capacity(); i <= c; ++i)
    v.insert(v.begin(), v.front());

  std::vector<int, Alloc<int>> va;
  va.insert(va.begin(), 99);
  va.insert(va.begin(), va.front());
  VERIFY( va.size() == 2 );
  va.erase(va.begin());

  return true;
}

static_assert( test_insert_erase() );

constexpr bool
test_clear()
{
  std::vector<int> v0;
  v0.clear();
  VERIFY( v0.size() == 0 );
  VERIFY( v0.capacity() == 0 );

  std::vector<int> v{1, 10, 100};
  v.clear();
  VERIFY( v.size() == 0 );
  VERIFY( v.capacity() == 3 );

  std::vector<int, Alloc<int>> va;
  va.clear();
  va.push_back(1);
  va.clear();
  va.clear();

  return true;
}

static_assert( test_clear() );

constexpr bool
test_erasure()
{
  const char* names[] = { "Vince", "Clarke", "Andy", "Bell" };
  std::vector<const char*> e(std::begin(names), std::end(names));

  auto n = std::erase(e, names[0]);
  VERIFY( n == 1 );
  VERIFY( e.size() == 3 );
  n = std::erase_if(e, [](auto name) { return name[4] == '\0'; });
  VERIFY( n == 2 );
  VERIFY( e.size() == 1 );

  return true;
}

static_assert( test_erasure() );
