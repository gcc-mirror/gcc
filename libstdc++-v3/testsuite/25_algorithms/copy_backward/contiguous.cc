// { dg-do run { target c++20 } }

#include <algorithm>
#include <iterator>
#include <cstdlib>
#include <testsuite_hooks.h>

const int valid_size = 3;
const int out_of_bound = valid_size + 1;
// array is larger than valid_size so that `data + out_of_bound` and `data - 1`
// do not have undefined behaviour, but data[valid_size] and data[-1] are
// not allowed to be accessed by Iter.
int array[1 + valid_size + 1]{ -999, 1, 2, 3, -999 };
int* data = array + 1;

struct Iter
{
  using iterator_category = std::contiguous_iterator_tag;
  using value_type = int;
  using different_type = int;
  using reference = int&;
  using pointer = int*;

  static inline bool advance_error = false;
  static inline bool address_error = false;

  int index{};

  int& operator*() const
  { std::abort(); } // Should not happen if reads/writes are done on pointers.

  int* operator->() const
  {
    if (index < 0 || index > valid_size)
    {
      address_error = true;
      return data;
    }
    return data + index;
  }

  int& operator[](int n) const { return *(*this + n); }

  Iter& operator++() { return *this += 1; }
  Iter& operator--() { return *this -= 1; }
  Iter operator++(int) { return Iter{index++}; }
  Iter operator--(int) { return Iter{index--}; }

  bool operator==(const Iter&) const = default;
  auto operator<=>(const Iter&) const = default;

  Iter& operator+=(int n)
  {
    index += n;
    if (index < 0 || index > valid_size)
      advance_error = true;
    return *this;
  }
  Iter& operator-=(int n) { return *this += -n; }

  friend Iter operator+(Iter i, int n) { return i += n; }
  friend Iter operator+(int n, Iter i) { return i + n; }
  friend Iter operator-(Iter i, int n) { return i + -n; }
  friend int operator-(Iter i, Iter j) { return i.index - j.index; }
};

static_assert( std::contiguous_iterator<Iter> );

int main()
{
  // P3349R1 allows std::copy_backward to lower contiguous iterators to pointers
  // but it must still advance the contiguous iterator and use std::to_address
  // to get a pointer for both ends of the range.
  // This test verifies that Iter::operator-> is called for an out-of-range
  // iterator, so that a hardened iterator with error-checking is able to
  // detect the error.

  int i[out_of_bound];
  // Attempt to read from an out-of-bound Iter:
  std::copy_backward(Iter{0}, Iter{out_of_bound}, i + out_of_bound);
  VERIFY( Iter::advance_error );
  VERIFY( Iter::address_error );
  Iter::advance_error = Iter::address_error = false;
  // Attempt to write to an out-of-bound Iter with index -1:
  std::copy_backward(std::begin(i), std::end(i), Iter{valid_size});
  VERIFY( Iter::advance_error );
  VERIFY( Iter::address_error );
}
