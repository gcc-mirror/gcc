// { dg-do run }
// { dg-require-normal-mode "debug mode checks use operator-(Iter, Iter)" }

#include <algorithm>
#include <iterator>
#include <testsuite_hooks.h>

const int g = 0;

struct Iter {
  typedef long difference_type;
  typedef int value_type;
  typedef const int& reference;
  typedef const int* pointer;
#if __cpp_lib_concepts
  using iterator_category = std::contiguous_iterator_tag;
#else
  typedef std::random_access_iterator_tag iterator_category;
#endif

  Iter(const int* p = 0, const int* limit = 0) : ptr(p), limit(limit) { }

  const int& operator*() const {
#ifdef __cpp_exceptions
    if (ptr == limit)
      throw 1;
#endif
    return *ptr;
  }
  const int* operator->() const { return ptr; }
  const int& operator[](long n) const { return ptr[n]; }

  Iter& operator++() { ++ptr; return *this; }
  Iter operator++(int) { Iter tmp = *this; ++ptr; return tmp; }
  Iter& operator--() { --ptr; return *this; }
  Iter operator--(int) { Iter tmp = *this; --ptr; return tmp; }

  Iter& operator+=(int n) { ptr += n; return *this; }
  Iter& operator-=(int n) { ptr -= n; return *this; }

  friend Iter operator+(int n, Iter it) { return it += n; }
  friend Iter operator+(Iter it, int n) { return it += n; }
  friend Iter operator-(Iter it, int n) { return it -= n; }

  bool operator==(const Iter& it) const { return ptr == it.ptr; }
  bool operator!=(const Iter& it) const { return ptr != it.ptr; }
  bool operator<(const Iter& it) const { return ptr < it.ptr; }
  bool operator>(const Iter& it) const { return ptr > it.ptr; }
  bool operator<=(const Iter& it) const { return ptr <= it.ptr; }
  bool operator>=(const Iter& it) const { return ptr >= it.ptr; }

  // std::copy should not need to take the difference between two iterators:
  friend int operator-(Iter, Iter) { VERIFY( ! "operator- called" ); }

private:
  const int* ptr;
  const int* limit;
};

void
test_pr115444_no_difference()
{
  int from = 1;
  int to = 0;
  Iter iter(&from);
  // This should not use operator-(Iter, Iter)
  std::copy(iter, iter+1, &to);
}

void
test_pr115444_exceptional()
{
#if __cpp_exceptions
  int from[3] = { 1, 2, 3 };
  int to[3] = { -1, -1, -1 };
  Iter iter(from, from+2);
  try {
    std::copy(iter, iter + 3, to);
  } catch (int) {
  }
  // std::copy should exit via exception on third dereference.
  // This precludes using memcpy or memmove to optimize the copying.
  VERIFY( to[0] == 1 );
  VERIFY( to[1] == 2 );
  VERIFY( to[2] == -1 );
#endif
}

int main()
{
  test_pr115444_no_difference();
  test_pr115444_exceptional();
}
