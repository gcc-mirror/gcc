// { dg-do run { target c++14 } }

#include <experimental/propagate_const>
#include <testsuite_hooks.h>

using std::experimental::propagate_const;

void
test_base_conversion()
{
  struct Base { };
  struct Derived : Base { };

  static_assert(std::is_convertible<propagate_const<Derived*>, Base*>::value,
      "PR libstdc++/107525 - SFINAE breaks conversion operators");
  static_assert(std::is_convertible<const propagate_const<Derived*>, const Base*>::value,
      "PR libstdc++/107525 - SFINAE breaks conversion operators");
}

void
test_const_conversion()
{
  struct X
  {
    int* p = nullptr;

    int& operator*() const { return *p; }
    int* operator->() const { return p; }
    int* get() const { return p; }

    operator int*() { return p; }
    operator const int*() const = delete;
  };

  static_assert(!std::is_convertible<const X, const int*>::value,
		"Cannot convert const X to const int*");
  // So should not be able to convert const propagate_const<X> to const int*.
  static_assert(!std::is_convertible<const propagate_const<X>, const int*>::value,
		"So should not be able to convert const propagate_const<X> to "
		"const int* (although this is not what LFTSv3 says)");
}

int main()
{
  test_base_conversion();
  test_const_conversion();
}
