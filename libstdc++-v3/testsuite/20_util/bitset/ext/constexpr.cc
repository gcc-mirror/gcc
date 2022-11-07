// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }
// This test relies on std::string.
// { dg-require-effective-target hosted }

#include <bitset>
#include <testsuite_hooks.h>

constexpr bool
test_find()
{
  VERIFY( std::bitset<0>()._Find_first() == 0 );
  VERIFY( std::bitset<1>()._Find_first() == 1 );
  VERIFY( std::bitset<55>("001000")._Find_first() == 3 );
  VERIFY( std::bitset<66>("101000")._Find_next(3) == 5 );
  return true;
}

static_assert( test_find() );

constexpr bool
test_unchecked()
{
  VERIFY( std::bitset<1>()._Unchecked_set(0).count() == 1 );
  VERIFY( std::bitset<44>()._Unchecked_set(3).count() == 1 );
  VERIFY( std::bitset<55>()._Unchecked_set(3, 0).count() == 0 );
  VERIFY( std::bitset<66>()._Unchecked_set(3, 1).count() == 1 );
  VERIFY( std::bitset<77>("111")._Unchecked_reset(1).count() == 2 );
  VERIFY( std::bitset<88>("101")._Unchecked_flip(1).count() == 3 );
  VERIFY( std::bitset<99>("010")._Unchecked_test(1) );
  return true;
}

static_assert( test_unchecked() );
