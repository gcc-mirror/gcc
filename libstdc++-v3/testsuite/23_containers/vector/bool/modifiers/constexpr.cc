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
  std::vector<bool> v;
  std::vector<bool>::reference r = v.emplace_back("");
  VERIFY( r == true );
  v.emplace_back(r);
  VERIFY( v.back() == true );
  v.emplace_back(v.front());
  VERIFY( v.back() == true );
  v.resize(64);
  v.emplace_back(v.back());
  VERIFY( v.back() == false );
  VERIFY( v.size() == 65 );
  v.emplace_back(8);
  VERIFY( v.size() == 66 );
  VERIFY( v.back() == true );
  v.emplace_back();
  VERIFY( v.size() == 67 );
  VERIFY( v.back() == false );

  v.pop_back();
  VERIFY( v.size() == 66 );
  VERIFY( v.back() == true );
  for (int i = 0, n = v.size(); i < n; ++i)
    v.pop_back();
  VERIFY( v.empty() );

  v.push_back(true);
  for (std::size_t i = 0, c = v.capacity(); i <= c; ++i)
    v.push_back(v.front());
  VERIFY( v.capacity() > v.size() );

  std::vector<bool, Alloc<bool>> va;
  va.push_back(true);
  va.push_back(va.front());
  VERIFY( va.size() == 2 );

  return true;
}

static_assert( test_push_back() );

template<typename T = bool>
constexpr std::false_type
pop_back_empty() { return {}; }

template<typename T = bool>
requires (std::bool_constant<(std::vector<T>().pop_back(), true)>::value)
constexpr std::true_type
pop_back_empty() { return {}; }

static_assert( ! pop_back_empty() );

constexpr bool
test_insert_erase()
{
  std::vector<bool> v;

  // vector::emplace(const_iterator, Args&&...)
  auto p = v.emplace(v.begin());
  VERIFY( p == v.begin() );
  p = v.emplace(v.end(), '7');
  VERIFY( p == --v.end() );

  // vector::insert(const_iterator, const T&)
  p = v.insert(v.begin(), *p);
  VERIFY( p == v.begin() );
  VERIFY( *p );
  // vector::insert(const_iterator, T&&)
  p = v.insert(v.end(), 1);
  VERIFY( p == --v.end() );
  v.insert(p, v.front());
  v.insert(v.end(), v.back());
  VERIFY( v.size() == 6 );
  v.insert(v.end(), true);
  VERIFY( v.size() == 7 );
  VERIFY( v.back() == true );

  // vector::insert(const_iterator, size_type, const T&)
  v.insert(v.begin(), 2, v.front());
  v.insert(v.end(), 3, 99);
  VERIFY( v.size() == 12 );

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

  // vector::insert(const_iterator, Iter, Iter);
  v.insert(v.begin() + 2, input_iterator(), input_iterator());
  VERIFY( v.size() == 12 );
  v.insert(v.end() - 9, input_iterator(18), input_iterator());
  VERIFY( v.size() == 30 );
  short a[] = { false, true };
  v.insert(v.end(), a, a + 2);
  VERIFY( v.size() == 32 );

  // vector::insert(const_iterator, initializer_list<T>)
  v.insert(v.begin(), {1,1,1});
  VERIFY( v.size() == 35 );

  // vector::erase(const_iterator)
  v.erase(v.end() - 1);
  VERIFY( v.size() == 34 );
  VERIFY( v.back() == false );
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

  std::vector<bool, Alloc<bool>> va;
  va.insert(va.begin(), 99);
  va.insert(va.begin(), va.front());
  VERIFY( va.size() == 2 );
  va.erase(va.begin());

  return true;
}

static_assert( test_insert_erase() );

constexpr std::size_t
capacity_for(std::size_t n)
{
  std::size_t N = std::vector<bool>(1).capacity();
  if (auto r = n % N)
    return n - r + N;
  return n;
}

constexpr bool
test_clear()
{
  std::vector<bool> v0;
  v0.clear();
  VERIFY( v0.size() == 0 );
  VERIFY( v0.capacity() == 0 );

  std::vector<bool> v{1, 0, 0};
  v.clear();
  VERIFY( v.size() == 0 );
  VERIFY( v.capacity() == capacity_for(3) );

  std::vector<bool, Alloc<bool>> va;
  va.clear();
  va.push_back(1);
  va.clear();
  va.clear();

  return true;
}

static_assert( test_clear() );

constexpr bool
test_flip()
{
  std::vector<bool> v{1, 0, 0, 1, 0, 1, 1, 0};
  v.flip();
  VERIFY( !v[0] && v[1] && v[2] && !v[3] && v[4] && !v[5] && !v[6] && v[7] );
  v[2].flip();
  VERIFY( !v[2] );
  v[2].flip();
  VERIFY( v[2] );

  return true;
}

static_assert( test_flip() );

constexpr bool
test_erasure()
{
  std::vector<bool> e{true,true,true,false,true,false};

  auto n = std::erase(e, false);
  VERIFY( n == 2 );
  VERIFY( e.size() == 4 );
  e[2] = false;
  n = std::erase_if(e, [](std::vector<bool>::reference val) { return !val; });
  VERIFY( n == 1 );
  VERIFY( e.size() == 3 );

  return true;
}

static_assert( test_erasure() );
