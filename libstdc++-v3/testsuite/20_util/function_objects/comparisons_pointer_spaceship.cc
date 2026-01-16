// { dg-do run { target c++20 } }

#include <compare>
#include <cstring>
#include <functional>
#include <testsuite_hooks.h>

constexpr const char arr[] = "efgh\0abcd\0ijkl";
constexpr const char* s1 = arr;
constexpr const char* s2 = arr+5;
constexpr const char* s3 = arr+6;

struct CStrNone
{
  const char* str;

  constexpr
  operator const char*() const
  { return str; }
};

template<typename ResultCreator>
struct CStrMem
{
  const char* str;

  constexpr
  operator const char*() const
  { return str; }

  auto operator<=>(CStrMem const& rhs) const
  { return ResultCreator::create(std::strcmp(this->str, rhs.str)); }

  auto operator<=>(const char* rhs) const
  { return ResultCreator::create(std::strcmp(this->str, rhs)); }
};

template<typename ResultCreator>
struct CStrFriend
{
  const char* str;

  constexpr
  operator const char*() const
  { return str; }

  friend auto operator<=>(CStrFriend lhs, CStrFriend rhs)
  { return ResultCreator::create(std::strcmp(lhs.str, rhs.str)); }

  friend auto operator<=>(CStrFriend lhs, const char* rhs)
  { return ResultCreator::create(std::strcmp(lhs.str, rhs)); }
};

template<typename ResultCreator>
struct CStrFree
{
  const char* str;

  constexpr
  operator const char*() const
  { return str; }
};

template<typename RC>
auto operator<=>(CStrFree<RC> lhs, CStrFree<RC> rhs)
{ return RC::create(std::strcmp(lhs.str, rhs.str)); }

template<typename RC>
auto operator<=>(CStrFree<RC> lhs, const char* rhs)
{ return RC::create(std::strcmp(lhs.str, rhs)); }

template<typename ResultCreator>
struct CStrMixed
{
  const char* str;

  constexpr
  operator const char*() const
  { return str; }
};

template<typename RC>
auto operator<=>(CStrMixed<RC> lhs, CStrMixed<RC> rhs)
{ return RC::create(std::strcmp(lhs.str, rhs.str)); }

template<typename RC>
auto operator<=>(CStrMixed<RC> lhs, CStrFree<RC> rhs)
{ return RC::create(std::strcmp(lhs.str, rhs.str)); }

// If the type returned from shapeship does not support relational
// operators, then synthesized operators are ill-formed, SFINAEable.
struct ReturnVoid
{
  constexpr static void
  create(int) { }
};

struct NoOperators
{
  constexpr static NoOperators
  create(int)
  { return NoOperators(); }
};

// std defined ordering types are expected
template<typename Ord = std::strong_ordering>
struct ReturnOrd
{
  constexpr static Ord
  create(int cmp)
  { return cmp <=> 0; }
};

// However, other types that provide required
// operators are supported.
struct ReturnInt
{
  constexpr static int
  create(int cmp)
  { return cmp; }
};

struct CustomOrd
{
  constexpr static CustomOrd
  create(int cmp)
  { return CustomOrd(cmp); }

  CustomOrd() = default;

  explicit constexpr
  CustomOrd(int cmp)
  : v(cmp) {}

  friend constexpr bool
  operator<(CustomOrd c, std::nullptr_t)
  { return c.v < 0; }

  friend constexpr bool
  operator<(std::nullptr_t, CustomOrd c)
  { return 0 < c.v; }

  friend constexpr bool
  operator>(CustomOrd c, std::nullptr_t)
  { return c.v > 0; }

  friend constexpr bool
  operator>(std::nullptr_t, CustomOrd c)
  { return 0 > c.v; }

  friend constexpr bool
  operator<=(CustomOrd c, std::nullptr_t)
  { return c.v <= 0; }

  friend constexpr bool
  operator<=(std::nullptr_t, CustomOrd c)
  { return 0 <= c.v; }

  friend constexpr bool
  operator>=(CustomOrd c, std::nullptr_t)
  { return c.v >= 0; }

  friend constexpr bool
  operator>=(std::nullptr_t, CustomOrd c)
  { return 0 >= c.v; }

  friend constexpr CustomOrd
  operator<=>(CustomOrd c, std::nullptr_t)
  { return c; }

  friend constexpr CustomOrd
  operator<=>(std::nullptr_t, CustomOrd c)
  { return CustomOrd(-c.v); }

private:
  int v = 0;
};

template<typename CStr1, typename CStr2>
void
test_relational(bool use_overloaded)
{
  CStr1 cs1{s1}; CStr2 cs2{s2};

  if (use_overloaded)
  {
    // Overloaded operaetors compare content of the string,
    // and cs1 > cs2;

    VERIFY( !(cs1 < cs2) );
    VERIFY( !std::less<>{}(cs1, cs2) );
    VERIFY( !std::ranges::less{}(cs1, cs2) );

    VERIFY( (cs1 > cs2) );
    VERIFY( std::greater<>{}(cs1, cs2) );
    VERIFY( std::ranges::greater{}(cs1, cs2) );

    VERIFY( !(cs1 < cs2) );
    VERIFY( !std::less_equal<>{}(cs1, cs2) );
    VERIFY( !std::ranges::less_equal{}(cs1, cs2) );

    VERIFY( (cs1 > cs2) );
    VERIFY( std::greater_equal<>{}(cs1, cs2) );
    VERIFY( std::ranges::greater_equal{}(cs1, cs2) );
  }
  else
  {
    // Without overloaded operators, we comapre pointers,
    // and cs1 < cs2;

    VERIFY( (cs1 < cs2) );
    VERIFY( std::less<>{}(cs1, cs2) );
    VERIFY( std::ranges::less{}(cs1, cs2) );

    VERIFY( !(cs1 > cs2) );
    VERIFY( !std::greater<>{}(cs1, cs2) );
    VERIFY( !std::ranges::greater{}(cs1, cs2) );

    VERIFY( (cs1 < cs2) );
    VERIFY( std::less_equal<>{}(cs1, cs2) );
    VERIFY( std::ranges::less_equal{}(cs1, cs2) );

    VERIFY( !(cs1 > cs2) );
    VERIFY( !std::greater_equal<>{}(cs1, cs2) );
    VERIFY( !std::ranges::greater_equal{}(cs1, cs2) );
  }
}

template<typename CStr>
void
test_relational_type(bool use_overloaded)
{
  test_relational<CStr, CStr>(use_overloaded);
  test_relational<CStr, const char*>(use_overloaded);
  test_relational<const char*, CStr>(use_overloaded);
}

template<typename RC>
void
test_relational_return()
{
  test_relational_type<CStrMem<RC>>(true);
  test_relational_type<CStrFriend<RC>>(true);
  test_relational_type<CStrFree<RC>>(true);
  test_relational<CStrMixed<RC>, CStrFree<RC>>(true);
  test_relational<CStrFree<RC>, CStrMixed<RC>>(true);
}

template<typename CStr1, typename CStr2>
void
test_spaceship(bool use_overloaded)
{
  CStr1 cs1{s1}; CStr2 cs2{s2};

  if (use_overloaded)
  {
    // Overloaded operaetors compare content of the string,
    // and cs1 > cs2;
    VERIFY( (cs1 <=> cs2) > 0 );
    VERIFY( std::compare_three_way{}(cs1, cs2) > 0 );
  }
  else
  {
    // Without overloaded operators, we comapre pointers,
    // and cs1 < cs2;
    VERIFY( (cs1 <=> cs2) < 0 );
    VERIFY( std::compare_three_way{}(cs1, cs2) < 0 );
  }
}

template<typename CStr>
void
test_spaceship_type(bool use_overloaded)
{
  test_spaceship<CStr, CStr>(use_overloaded);
  test_spaceship<CStr, const char*>(use_overloaded);
  test_spaceship<const char*, CStr>(use_overloaded);
}

template<typename Ordering>
void
test_std_ordering()
{
  using RC = ReturnOrd<Ordering>;
  test_relational_return<RC>();
  test_spaceship_type<CStrMem<RC>>(true);
  test_spaceship_type<CStrFriend<RC>>(true);
  test_spaceship_type<CStrFree<RC>>(true);
  test_spaceship<CStrMixed<RC>, CStrFree<RC>>(true);
  test_spaceship<CStrFree<RC>, CStrMixed<RC>>(true);
}

template<typename CStr1, typename CStr2>
void
test_no_relational()
{
  CStr1 c1{}; CStr2 c2{};
  static_assert(!requires { c1 < c2; });
  static_assert(!requires { c1 > c2; });
  static_assert(!requires { c1 <= c2; });
  static_assert(!requires { c1 >= c2; });
}

template<typename CStr>
void
test_no_relational_type()
{
  test_no_relational<CStr, CStr>();
  test_no_relational<CStr, const char*>();
  test_no_relational<const char*, CStr>();
}

template<typename RC>
void
test_no_relational_return()
{
  test_no_relational_type<CStrMem<RC>>();
  test_no_relational_type<CStrFriend<RC>>();
  test_no_relational_type<CStrFree<RC>>();
  test_no_relational<CStrMixed<RC>, CStrFree<RC>>();
  test_no_relational<CStrFree<RC>, CStrMixed<RC>>();
}

int main()
{
  test_std_ordering<std::strong_ordering>();
  test_std_ordering<std::weak_ordering>();
  test_std_ordering<std::partial_ordering>();

  test_relational_type<CStrNone>(false);
  test_relational_return<ReturnInt>();
  test_relational_return<CustomOrd>();

  test_no_relational_return<ReturnVoid>();
  test_no_relational_return<NoOperators>();
}
