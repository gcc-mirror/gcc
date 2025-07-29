// { dg-do run { target c++26 } }

#include <inplace_vector>

#include <span>
#include <testsuite_hooks.h>

template<size_t N, typename T>
constexpr void
test_equal(size_t c)
{
  T a[]{1, 2, 3, 4, 5, 6, 7, 8, 9};
  std::inplace_vector<T, N> v(a, a+c);
  VERIFY(   v == v  );
  VERIFY( !(v != v) );
  VERIFY( !(v <  v) );
  VERIFY( !(v >  v) );
  VERIFY(   v <= v  );
  VERIFY(   v >= v  );
  VERIFY( (v <=> v) == 0 );
}

template<typename T>
constexpr void
test_not_equal()
{
  std::inplace_vector<T, 10> v3l{T{1}, T{2}, T{3}};
  std::inplace_vector<T, 10> v3g{T{1}, T{3}, T{3}};
  VERIFY( !(v3l == v3g) );
  VERIFY(   v3l != v3g  );
  VERIFY(   v3l <  v3g  );
  VERIFY( !(v3l >  v3g) );
  VERIFY(   v3l <= v3g  );
  VERIFY( !(v3l >= v3g) );
  VERIFY( (v3l <=> v3g) < 0 );

  std::inplace_vector<T, 10> v2{T{1}, T{2}};
  VERIFY( !(v2 == v3l) );
  VERIFY(   v2 != v3l  );
  VERIFY(   v2 <  v3l  );
  VERIFY( !(v2 >  v3l) );
  VERIFY(   v2 <= v3l  );
  VERIFY( !(v2 >= v3l) );
  VERIFY( (v2 <=> v3l) < 0 );
}

int main()
{
  auto test_all = [] {
    test_equal<0, int>(0);
    test_equal<4, int>(0);
    test_equal<4, int>(2);
    test_equal<4, int>(4);
    test_not_equal<int>();
    return true;
  };

  test_all();
  static_assert(test_all());;
}
