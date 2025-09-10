// { dg-do compile { target c++11 } }

#include <algorithm>
#include <testsuite_iterators.h>

struct Iter
{
  using value_type = int;
  using difference_type = short;
  using iterator_category = std::random_access_iterator_tag;
  using pointer = const value_type*;
  using reference = const value_type&;

  Iter() : p(nullptr) { }
  explicit Iter(pointer p) : p(p) { }
  reference operator*() const { return *p; }
  pointer operator->() const { return p; }
  reference operator[](difference_type n) const { return p[n]; }
  Iter& operator++() { ++p; return *this; }
  Iter& operator--() { --p; return *this; }
  Iter operator++(int) { return Iter(p++); }
  Iter operator--(int) { return Iter(p--); }
  Iter& operator+=(difference_type n) { p += n; return *this; }
  Iter& operator-=(difference_type n) { p -= n; return *this; }

  friend Iter operator+(Iter i, difference_type n) { return i += n; }
  friend Iter operator+(difference_type n, Iter i) { return i += n; }
  friend Iter operator-(Iter i, difference_type n) { return i -= n; }
  friend difference_type operator-(Iter i, Iter j) { return i.p - j.p; }

  template<typename D> void operator[](D) const = delete;
  template<typename D> void operator+=(D) = delete;
  template<typename D> void operator-=(D) = delete;
  template<typename D> friend void operator+(Iter, difference_type) = delete;
  template<typename D> friend void operator+(difference_type, Iter) = delete;
  template<typename D> friend void operator-(Iter, difference_type) = delete;

  friend bool operator==(Iter i, Iter j) { return i.p == j.p; }
  friend bool operator!=(Iter i, Iter j) { return i.p != j.p; }
  friend bool operator<(Iter i, Iter j) { return i.p < j.p; }
  friend bool operator<=(Iter i, Iter j) { return i.p <= j.p; }
  friend bool operator>(Iter i, Iter j) { return i.p > j.p; }
  friend bool operator>=(Iter i, Iter j) { return i.p >= j.p; }

private:
  pointer p;
};

void
test_pr121890()
{
  // algorithms do not use iterator's difference_type for arithmetic
  int a[1] = { };
  __gnu_test::random_access_container<int> c(a);
  (void) std::lexicographical_compare(c.begin(), c.end(), Iter(a), Iter(a+1));
  (void) std::lexicographical_compare(Iter(a), Iter(a+1), c.begin(), c.end());
}
