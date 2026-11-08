// { dg-do run }

#include <vector>
#include <ranges>
#include <stdexcept>
#include <testsuite_hooks.h>

static void
test_n_element_ctor()
{
  try {
    std::vector<bool> v(-15, true);
    VERIFY(false);
  } catch (const std::length_error&) {
  }
}

static void
test_n_element_ctor_dispatch()
{
  try {
    std::vector<bool> v(-15, 1); // in C++98 this calls vector(Iter, Iter) ctor
    VERIFY(false);
  } catch (const std::length_error&) {
  }
}

// A Cpp17RandomAccessIterator for which std::distance always returns an
// impossibly large distance. This implements the Cpp17RandomAccessIterator
// API but doesn't meet the semantic requirements.
// For the test below, all that matters is that the huge value from
// std::distance(first, last) causes std::length_error to be thrown.
struct FakeIterator
{
  typedef std::random_access_iterator_tag iterator_category;
  typedef bool value_type;
  typedef std::size_t difference_type; // should be a signed type
  typedef const bool* pointer;
  typedef const bool& reference;

  FakeIterator() : val(true) { }

  reference operator*() const { return val; }
  pointer   operator->() const { return &val; }
  reference operator[](difference_type) const { return val; }

  FakeIterator& operator++() { return *this; }
  FakeIterator  operator++(int) { return *this; }
  FakeIterator& operator--() { return *this; }
  FakeIterator  operator--(int) { return *this; }

  FakeIterator& operator+=(difference_type) { return *this; }
  FakeIterator& operator-=(difference_type) { return *this; }

  FakeIterator operator+(difference_type) const { return *this; }
  FakeIterator operator-(difference_type) const { return *this; }

  friend FakeIterator operator+(difference_type, FakeIterator i) { return i; }

  // Return an impossibly large distance
  difference_type operator-(FakeIterator) const { return (std::size_t)-8; }

  // These are meaningless results but not actually used by this test.
  bool operator==(const FakeIterator&) const { return false; }
  bool operator!=(const FakeIterator&) const { return true; }
  bool operator<(const FakeIterator&) const { return true; }
  bool operator>(const FakeIterator&) const { return false; }
  bool operator<=(const FakeIterator&) const { return true; }
  bool operator>=(const FakeIterator&) const { return false; }

private:
  bool val;
};

#ifdef __cpp_lib_ranges
// Wraps a std::random_access_iterator as a Cpp17RandomAccessIterator
// except for having an integer-like class type as difference_type.
// This one is _almost_ a real Cpp17RandomAccessIterator, except for the
// difference_type. Again, we only care that the huge distance is
// detected and std::length_error is thrown.
class Iterator
{
  using View = std::ranges::iota_view<std::size_t, std::size_t>;
  using ViewIter = std::ranges::iterator_t<View>;

public:
  using iterator_category = std::random_access_iterator_tag;
  using value_type = bool;
  using difference_type = std::iter_difference_t<ViewIter>;

  Iterator() = default;

  explicit Iterator(ViewIter iter) : iter(iter) { }

  const bool& operator*() const { return val; }
  const bool* operator->() const { return &val; }
  const bool& operator[](difference_type n) const { return *(*this + n); }

  Iterator& operator++() { ++iter; return *this; }
  Iterator  operator++(int) { return Iterator(iter++); }
  Iterator& operator--() { --iter; return *this; }
  Iterator  operator--(int) { return Iterator(iter--); }

  Iterator& operator+=(difference_type n) { iter += n; return *this; }
  Iterator& operator-=(difference_type n) { iter -= n; return *this; }

  Iterator operator+(difference_type n) const { return Iterator(iter + n); }
  Iterator operator-(difference_type n) const { return Iterator(iter - n); }

  friend Iterator operator+(difference_type n, Iterator i) { return i + n; }
  difference_type operator-(Iterator i) const { return iter - i.iter; }

  bool operator==(const Iterator& i) const { return iter == i.iter; }
  auto operator<=>(const Iterator& i) const { return iter <=> i.iter; }

private:
  ViewIter iter = ViewIter();
  bool val = true;
};

static_assert( std::random_access_iterator<Iterator> );
#endif

static void
test_range_ctors()
{
  try {
    FakeIterator x;
    std::vector<bool> v(x, x);
    VERIFY(false);
  } catch (const std::length_error&) {
  }

#ifdef __cpp_lib_ranges
  const std::size_t min = 0, max = -8;
  auto i = std::views::iota(min, max);
  try {
    Iterator first(std::ranges::begin(i));
    Iterator last(std::ranges::end(i));
    std::vector<bool> v(first, last);
    VERIFY(false);
  } catch (const std::length_error&) {
  }

#ifdef __cpp_lib_containers_ranges
  try {
    std::vector<bool> v(std::from_range, i);
    VERIFY(false);
  } catch (const std::length_error&) {
  }
#endif
#endif
}

int main()
{
  test_n_element_ctor();
  test_n_element_ctor_dispatch();
  test_range_ctors();
}
