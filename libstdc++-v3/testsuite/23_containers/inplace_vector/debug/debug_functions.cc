// { dg-do run { target c++26 } }
// { dg-require-debug-mode "" }

#include <inplace_vector>
#include <testsuite_hooks.h>

void test02()
{
  using namespace __gnu_debug;

  std::inplace_vector<int, 10> v1(3, 1);
  VERIFY( !__check_singular(v1.begin()) );
  auto it = v1.begin();
  VERIFY( !__check_singular(it) );

  VERIFY( !__check_singular(v1.end()) );
  it = v1.end();
  VERIFY( !__check_singular(it) );

  v1.clear();

  VERIFY( it._M_singular() );
  VERIFY( __check_singular(it) );

  it = v1.end();
  VERIFY( !it._M_singular() );
  VERIFY( !__check_singular(it) );
}

int main()
{
  test02();
  return 0;
}
