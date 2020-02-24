// Copyright (C) 2020 Free Software Foundation, Inc.
//
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

struct X
{
  int a = -1;
  bool moved_from = false;

  X() = default;

  X(int a)
    : a(a)
  { }

  X(const X&) = delete;
  X& operator=(const X&) = delete;

  X(X&& other)
  {
    if (this != &other)
    *this = std::move(other);
  }

  X&
  operator=(X&& other)
  {
    a = other.a;
    other.moved_from = true;
    moved_from = false;
    return *this;
  }
};

template<int N, template<typename> typename Wrapper>
void
test01()
{
  for (int n = 0; n < N+5; n++)
    {
      X x[N];
      for (int i = 0; i < N; i++)
	x[i] = X{i};
      test_container<X, Wrapper> cx(x);
      auto out = std::shift_left(cx.begin(), cx.end(), n);
      if (n < N)
	{
	  VERIFY( out.ptr == x+(N-n) );
	  for (int i = 0; i < N-n; i++)
	    {
	      VERIFY( x[i].a == n+i );
	      VERIFY( !x[i].moved_from );
	    }
	  for (int i = std::max(n, N-n); i < N; i++)
	    VERIFY( x[i].moved_from );
	}
      else
	{
	  VERIFY( out.ptr == x );
	  for (int i = 0; i < N; i++)
	    {
	      VERIFY( x[i].a == i );
	      VERIFY( !x[i].moved_from );
	    }
	}
    }
}

int
main()
{
  test01<23, forward_iterator_wrapper>();
  test01<23, bidirectional_iterator_wrapper>();
  test01<23, random_access_iterator_wrapper>();

  test01<24, forward_iterator_wrapper>();
  test01<24, bidirectional_iterator_wrapper>();
  test01<24, random_access_iterator_wrapper>();
}
