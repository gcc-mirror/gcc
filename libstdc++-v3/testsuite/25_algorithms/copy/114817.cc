// { dg-do run { target c++11 } }

// Bug libstdc++/114817 - Wrong codegen for std::copy of
// "trivially copyable but not trivially assignable" type

#include <algorithm>
#include <testsuite_hooks.h>

int assignments = 0;

struct NonTrivialAssignment
{
  NonTrivialAssignment(int v) : val(v) { }
  NonTrivialAssignment(const NonTrivialAssignment&) = default;
  void operator=(const volatile NonTrivialAssignment&) = delete;
  template<class = void>
    NonTrivialAssignment&
    operator=(const NonTrivialAssignment& o)
    {
      ++assignments;
      val = o.val;
      return *this;
    }

  int val;
};

static_assert(std::is_trivially_copyable<NonTrivialAssignment>::value);

int main()
{
  NonTrivialAssignment src[2]{1, 2};
  NonTrivialAssignment dst[2]{3, 4};
  std::copy(src, src+2, dst);
  VERIFY( assignments == 2 );
  VERIFY( dst[0].val == src[0].val );
  VERIFY( dst[1].val == src[1].val );
}
