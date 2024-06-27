// { dg-do run { target c++11 } }

// Bug libstdc++/114817 - Wrong codegen for std::copy of
// "trivially copyable but not trivially assignable" type

#include <memory>
#include <testsuite_hooks.h>

int constructions = 0;

struct NonTrivialCons
{
  NonTrivialCons() = default;
  NonTrivialCons(int v) : val(v) { }
  NonTrivialCons(const volatile NonTrivialCons&) = delete;
  template<class = void>
    NonTrivialCons(const NonTrivialCons& o)
    : val(o.val)
    {
      ++constructions;
    }

  int val;
};

static_assert(std::is_trivially_copyable<NonTrivialCons>::value);

int main()
{
  NonTrivialCons src[2]{1, 2};
  NonTrivialCons dst[2];
#if __cplusplus < 201703L
  constructions = 0;
#endif
  std::uninitialized_copy(src, src+2, dst);
  VERIFY( constructions == 2 );
  VERIFY( dst[0].val == src[0].val );
  VERIFY( dst[1].val == src[1].val );
}
