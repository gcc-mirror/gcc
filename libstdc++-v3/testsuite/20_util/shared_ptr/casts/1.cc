// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2006-2022 Free Software Foundation, Inc.
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

// 20.6.6.2.10 shared_ptr casts [util.smartptr.shared.cast]

#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

struct MyP { virtual ~MyP() { }; };
struct MyDP : MyP { };

void
test01()
{
  using __gnu_test::check_ret_type;
  using std::shared_ptr;
  using std::static_pointer_cast;
  using std::const_pointer_cast;
  using std::dynamic_pointer_cast;

  shared_ptr<double> spd;
  shared_ptr<const int> spci;
  shared_ptr<MyP> spa;

  check_ret_type<shared_ptr<void>>(static_pointer_cast<void>(spd));
  check_ret_type<shared_ptr<int>>(const_pointer_cast<int>(spci));
  // Non-polymorphic dynamic_cast works without RTTI.
  check_ret_type<shared_ptr<MyP>>(dynamic_pointer_cast<MyP>(spa));
#if __cpp_rtti
  // But polymorphic dynamic_cast needs RTTI.
  check_ret_type<shared_ptr<MyDP>>(dynamic_pointer_cast<MyDP>(spa));
#endif
}

void
test02()
{
  using std::shared_ptr;
  using std::static_pointer_cast;
  using std::const_pointer_cast;
  using std::dynamic_pointer_cast;

  int* ptr = new int(1);
  shared_ptr<const void> pcv(ptr);
  auto pci = static_pointer_cast<const int>(pcv);
  VERIFY(pci.use_count() == 2);
  VERIFY(pcv.use_count() == 2);
  VERIFY(pci.get() == ptr);
  VERIFY(pcv.get() == ptr);
  auto pi = const_pointer_cast<int>(pci);
  VERIFY(pi.use_count() == 3);
  VERIFY(pcv.use_count() == 3);
  VERIFY(pi.get() == ptr);
  VERIFY(pci.get() == ptr);

#if __cpp_rtti
  MyP* pptr = new MyP;
  shared_ptr<MyP> pp(pptr);
  auto pdp = dynamic_pointer_cast<MyDP>(pp);
  VERIFY(pp.use_count() == 1);
  VERIFY(pdp.use_count() == 0);
  VERIFY(pdp.get() == nullptr);
  VERIFY(pp.get() == pptr);
  pptr = new MyDP;
  pp.reset(pptr);
  pdp = dynamic_pointer_cast<MyDP>(pp);
  VERIFY(pp.use_count() == 2);
  VERIFY(pdp.use_count() == 2);
  VERIFY(pdp.get() == pptr);
  VERIFY(pp.get() == pptr);
#endif
}

int main()
{
  test01();
  test02();
}
