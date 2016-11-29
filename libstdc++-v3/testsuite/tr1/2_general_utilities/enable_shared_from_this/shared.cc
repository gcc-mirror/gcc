// Copyright (C) 2005-2016 Free Software Foundation, Inc.
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

// 2.2.5 Template class enable_shared_from_this [tr.util.smartptr.enab]

#include <tr1/memory>
#include <testsuite_hooks.h>

struct X : public std::tr1::enable_shared_from_this<X>
{
};

int
test01()
{
  std::tr1::shared_ptr<X> p(new X);
  std::tr1::shared_ptr<X>  q = p->shared_from_this();
  VERIFY( p == q );
  VERIFY( !(p < q) && !(q < p) );  // p and q share ownership

  return 0;
}

int
main()
{
  test01();
  return 0;
}
