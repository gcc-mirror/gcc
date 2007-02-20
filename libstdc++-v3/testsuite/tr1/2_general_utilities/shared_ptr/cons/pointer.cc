// Copyright (C) 2005 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// TR1 2.2.2 Template class shared_ptr [tr.util.smartptr.shared]

#include <tr1/memory>
#include <testsuite_hooks.h>

struct A { };
struct B : A { };


// 2.2.3.1 shared_ptr constructors [tr.util.smartptr.shared.const]

// Construction from pointer

int
test01()
{
  bool test __attribute__((unused)) = true;

  A * const a = 0;
  std::tr1::shared_ptr<A> p(a);
  VERIFY( p.get() == 0 );
  VERIFY( p.use_count() == 1 );

  return 0;
}

int
test02()
{
  bool test __attribute__((unused)) = true;

  A * const a = new A;
  std::tr1::shared_ptr<A> p(a);
  VERIFY( p.get() == a );
  VERIFY( p.use_count() == 1 );

  return 0;
}


int
test03()
{
  bool test __attribute__((unused)) = true;

  B * const b = new B;
  std::tr1::shared_ptr<A> p(b);
  VERIFY( p.get() == b );
  VERIFY( p.use_count() == 1 );

  return 0;
}

int
main()
{
  test01();
  test02();
  test02();
  return 0;
}
