// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-g0 -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe" }
// { dg-do run { target c++2a } }

#include <exception>
#include <cstdlib>
#define ASSERT_USES_CONTRACTS
#include <cassert>



struct X { explicit operator void*() const { return nullptr; } };

void
test_VERIFY(int i)
{
  // This should not be parsed as a function type bool(bool(i)):
  assert( bool(i) );

  // This should not produce warnings about lambda in unevaluated context:
  assert( []{ return 1; }() );

  // Only one expression allowed:
  assert(1, 2); // { dg-error "in expansion of macro" }
  // { dg-error "compound expression in functional cast" "" { target *-*-* } 0 }

  // A scoped enum is not contextually convertible to bool:
  enum class E { E0 };
  assert( E::E0 ); // { dg-error "could not convert" }

  // explicit conversion to void* is not contextually convertible to bool:
  X x;
  assert( x ); // { dg-error "in expansion of macro" }
  // { dg-error "invalid cast .* to type 'bool'" "" { target *-*-* } 0 }
}
