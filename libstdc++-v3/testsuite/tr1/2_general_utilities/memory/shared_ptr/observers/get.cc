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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// TR1 2.2.2 Template class shared_ptr [tr.util.smartptr.shared]

#include <tr1/memory>
#include <testsuite_hooks.h>

struct A
{
  A() : i() {}
  int i;
};

// 2.2.3.5 shared_ptr observers [tr.util.smartptr.shared.obs]

// get
int
test01()
{
  bool test __attribute__((unused)) = true;

  A * const a = new A;
  const std::tr1::shared_ptr<A> p(a);
  VERIFY( p.get() == a );

  return 0;
}

// operator*
int
test02()
{
  bool test __attribute__((unused)) = true;

  A * const a = new A;
  const std::tr1::shared_ptr<A> p(a);
  VERIFY( &*p == a );

  return 0;
}


// operator->
int
test03()
{
  bool test __attribute__((unused)) = true;

  A * const a = new A;
  const std::tr1::shared_ptr<A> p(a);
  VERIFY( &p->i == &a->i );

  return 0;
}


int 
main()
{
  test01();
  test02();
  test03();
  return 0;
}
