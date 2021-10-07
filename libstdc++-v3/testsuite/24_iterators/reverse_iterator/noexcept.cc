// { dg-do compile { target c++11 } }

#include <iterator>

template<typename T, bool Nothrow>
struct bidi
{
  using value_type = T;
  using pointer = T*;
  using reference = T&;
  using difference_type = std::ptrdiff_t;
  using iterator_category = std::bidirectional_iterator_tag;

  T* ptr;

  bidi(T* ptr = nullptr) noexcept(Nothrow) : ptr(ptr) { }

  bidi(const bidi& iter) noexcept(Nothrow) : ptr(iter.ptr) { }

  template<typename U>
    bidi(const bidi<U, Nothrow>& iter) noexcept(Nothrow) : ptr(iter.ptr) { }

  bidi& operator=(const bidi& iter) noexcept(Nothrow)
  {
    ptr = iter.ptr;
    return *this;
  }

  template<typename U>
  bidi& operator=(const bidi<U, Nothrow>& iter) noexcept(Nothrow)
  {
    ptr = iter.ptr;
    return *this;
  }

  bidi& operator++() { ++ptr; return *this; }
  bidi& operator--() { --ptr; return *this; }
  bidi operator++(int) { bidi tmp = *this; ++ptr; return tmp; }
  bidi operator--(int) { bidi tmp = *this; --ptr; return tmp; }

  reference operator*() const { return *ptr; }
  pointer operator->() const { return ptr; }
};

void
test01()
{
  using B1 = bidi<int, true>;
  using R1 = std::reverse_iterator<B1>;
  static_assert( std::is_nothrow_default_constructible<R1>(), "" );
  static_assert( std::is_nothrow_copy_constructible<R1>(), "" );
  static_assert( std::is_nothrow_move_constructible<R1>(), "" );
  static_assert( std::is_nothrow_copy_assignable<R1>(), "" );
  static_assert( std::is_nothrow_move_assignable<R1>(), "" );
  static_assert( std::is_nothrow_constructible<R1, const B1&>(), "" );
  static_assert( std::is_nothrow_constructible<R1, B1>(), "" );

  using B2 = bidi<const int, true>;
  using R2 = std::reverse_iterator<B2>;
  // Test conversions from reverse_iterator<B1> to reverse_iterator<B2>.
  static_assert( std::is_nothrow_constructible<R2, const R1&>(), "" );
  static_assert( std::is_nothrow_assignable<R2&, const R1&>(), "" );
  // And from B1 to reverse_iterator<B2>.
  static_assert( std::is_nothrow_constructible<R2, const B2&>(), "" );
  static_assert( std::is_nothrow_constructible<R2, B2>(), "" );
  static_assert( std::is_nothrow_constructible<R2, const B1&>(), "" );
  static_assert( std::is_nothrow_constructible<R2, B1>(), "" );

  using B3 = bidi<int, false>;
  using R3 = std::reverse_iterator<B3>;
  static_assert( ! std::is_nothrow_default_constructible<R3>(), "" );
  static_assert( ! std::is_nothrow_copy_constructible<R3>(), "" );
  static_assert( ! std::is_nothrow_move_constructible<R3>(), "" );
  static_assert( ! std::is_nothrow_copy_assignable<R3>(), "" );
  static_assert( ! std::is_nothrow_move_assignable<R3>(), "" );
  static_assert( ! std::is_nothrow_constructible<R3, const B3&>(), "" );
  static_assert( ! std::is_nothrow_constructible<R3, B3>(), "" );

  using B4 = bidi<const int, false>;
  using R4 = std::reverse_iterator<B4>;
  // Test conversions from reverse_iterator<B3> to reverse_iterator<B4>.
  static_assert( ! std::is_nothrow_constructible<R4, const R3&>(), "" );
  static_assert( ! std::is_nothrow_assignable<R4&, const R3&>(), "" );
  // And from B3 to reverse_iterator<B4>.
  static_assert( ! std::is_nothrow_constructible<R4, const B4&>(), "" );
  static_assert( ! std::is_nothrow_constructible<R4, B4>(), "" );
  static_assert( ! std::is_nothrow_constructible<R4, const B3&>(), "" );
  static_assert( ! std::is_nothrow_constructible<R4, B3>(), "" );

  static_assert( noexcept(std::declval<R1&>().base()), "" );
  static_assert( ! noexcept(std::declval<R3&>().base()), "" );
}
