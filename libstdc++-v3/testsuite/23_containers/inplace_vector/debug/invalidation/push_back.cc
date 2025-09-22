// { dg-do run { target c++26 } }

#include <debug/inplace_vector>
#include <testsuite_hooks.h>

using __gnu_debug::inplace_vector;

void test01()
{
  inplace_vector<int, 100> v(10, 17);

  auto before = v.begin() + 6;
  auto last = v.end();
  auto end = last--;

  v.push_back(42);

  VERIFY(before._M_dereferenceable());
  VERIFY(last._M_dereferenceable());
  VERIFY(end._M_singular());
}

#if __cpp_exceptions
void test02()
{
  inplace_vector<int, 10> v(10, 17);

  auto before = v.begin() + 6;
  auto last = v.end();
  auto end = last--;
  try
    {
      v.push_back(42);
      VERIFY( false );
    }
  catch (std::bad_alloc&)
    {
    }

  VERIFY(before._M_dereferenceable());
  VERIFY(last._M_dereferenceable());
  VERIFY(!end._M_singular());
}
#endif

int main()
{
  test01();
#if __cpp_exceptions
  test02();
#endif
  return 0;
}
