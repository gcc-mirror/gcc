// { dg-do run { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

#include <inplace_vector>

#include <ranges>
#include <testsuite_hooks.h>

template<typename Category>
struct iter_category_base
{ using iterator_category = Category; };

template<>
struct iter_category_base<void>
{ };

template<typename Concept>
struct iter_concept_base
{ using iterator_concept = Concept; };

template<>
struct iter_concept_base<void>
{ };

template<typename DifferenceType, bool Sized = true,
	 typename Category = std::random_access_iterator_tag,
	 typename Concept = Category>
struct iota_iterator
: iter_category_base<Category>, iter_concept_base<Concept>
{
  using value_type = DifferenceType;
  using difference_type = DifferenceType;
  using pointer = const DifferenceType*;
  using reference = DifferenceType;

  static constexpr bool sized = Sized;
  static inline std::size_t incrementCount = 0;
  static inline std::size_t decrement_count = 0;

  iota_iterator() = default;

  constexpr explicit
  iota_iterator(DifferenceType v)
  : value(v)
  { }

  constexpr DifferenceType
  operator*() const
  { return value; }

  constexpr DifferenceType
  operator[](difference_type n) const
  { return value + n; }

  constexpr iota_iterator&
  operator++()
  { ++incrementCount; ++value; return *this; }

  constexpr iota_iterator
  operator++(int)
  { ++incrementCount; auto tmp = *this; ++value; return tmp; }

  constexpr iota_iterator&
  operator--()
  { ++decrement_count; --value; return *this; }

  constexpr iota_iterator
  operator--(int)
  { ++decrement_count; auto tmp = *this; --value; return tmp; }

  constexpr iota_iterator&
  operator+=(difference_type n)
  { incrementCount += n; value += n; return *this; }

  constexpr iota_iterator&
  operator-=(difference_type n)
  { decrement_count += n; value -= n; return *this; }

  friend constexpr iota_iterator
  operator+(iota_iterator i, difference_type n)
  { return iota_iterator(i.value + n); }

  friend constexpr iota_iterator
  operator+(difference_type n, iota_iterator i)
  { return iota_iterator(i.value + n); }

  friend constexpr iota_iterator
  operator-(iota_iterator i, difference_type n)
  { return iota_iterator(i.value - n); }

  friend constexpr difference_type
  operator-(iota_iterator a, iota_iterator b)
    requires Sized
  { return a.value - b.value; }

  friend constexpr bool
  operator==(iota_iterator lhs, iota_iterator rhs) noexcept
  { return lhs.value == rhs.value; }

  friend constexpr auto
  operator<=>(iota_iterator lhs, iota_iterator rhs) noexcept
  { return lhs.value <=> rhs.value; }

private:
  DifferenceType value = {};
};

template<size_t N, typename Iterator>
constexpr void
do_test(Iterator first, Iterator last)
{
  std::inplace_vector<int, N> v;
  std::ranges::subrange<Iterator> range(first, last);

  v.clear();
  Iterator::incrementCount = 0;
  try
  {
    v.assign(first, last);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N+1 );

  v.clear();
  Iterator::incrementCount = 0;
  try
  {
    v.assign_range(range);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N+1 );

  v.clear();
  Iterator::incrementCount = 0;
  try
  {
    v.insert(v.begin(), first, last);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N+1 );

  v.clear();
  Iterator::incrementCount = 0;
  try
  {
    v.insert_range(v.begin(), range);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N+1 );

  v.clear();
  Iterator::incrementCount = 0;
  try
  {
    v.append_range(range);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N+1 );

  if constexpr (N < 4)
    return;

  v = {1, 2, 3, 4};
  Iterator::incrementCount = 0;
  try
  {
    v.assign(first, last);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N+1 );

  v = {1, 2, 3, 4};
  Iterator::incrementCount = 0;
  try
  {
    v.assign_range(range);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N+1 );

  v = {1, 2, 3, 4};
  Iterator::incrementCount = 0;
  try
  {
    v.insert(v.begin(), first, last);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N-3 );

  v = {1, 2, 3, 4};
  Iterator::incrementCount = 0;
  try
  {
    v.insert_range(v.begin(), range);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N-3 );

  v = {1, 2, 3, 4};
  Iterator::incrementCount = 0;
  try
  {
    v.append_range(range);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if (Iterator::sized)
    VERIFY( Iterator::incrementCount == 0 );
  else
    VERIFY( Iterator::incrementCount <= N-3 );
}

template<size_t N, typename DifferenceType, bool Sized,
	 typename Category, typename Concept>
constexpr void
test_iter()
{
  using Iterator = iota_iterator<DifferenceType, Sized, Category, Concept>;
  using Limits = std::numeric_limits<DifferenceType>;
  do_test<N>(Iterator(DifferenceType(0)), Iterator(Limits::max()));

  DifferenceType v2 = (DifferenceType(1) << (Limits::digits - 1)) + 2;
  do_test<N>(Iterator(DifferenceType(0)), Iterator(v2));
}

template<size_t N, bool Sized, typename Category, typename Concept>
constexpr void
test_difference()
{
  test_iter<N, int, Sized, Category, Concept>();
  using over_size_t = std::ranges::range_difference_t<std::ranges::iota_view<std::ptrdiff_t>>;
  test_iter<N, over_size_t, Sized, Category, Concept>();
}

template<size_t N, bool Sized, typename Category>
constexpr void
test_category()
{
  test_difference<N, Sized, Category, Category>();
  test_difference<N, Sized, void, Category>();
}

template<size_t N>
constexpr void
test_size()
{
  test_category<N, true, std::random_access_iterator_tag>();
  test_category<N, true, std::forward_iterator_tag>();
  test_category<N, true, std::input_iterator_tag>();
  test_category<N, false, std::forward_iterator_tag>();
  test_category<N, false, std::input_iterator_tag>();
}

int main()
{
  test_size<0>();
  test_size<4>();
  test_size<10>();
}
