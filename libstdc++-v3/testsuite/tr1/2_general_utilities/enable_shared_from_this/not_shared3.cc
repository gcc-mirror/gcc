// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

  X x;
  x = *p;  // copy of shared object should not be shared

  try
    {
      std::tr1::shared_ptr<X> p = x.shared_from_this();
      VERIFY( false );
    }
  catch (const std::tr1::bad_weak_ptr&)
    {
      // Expected.
      VERIFY( true );
    }
  catch (...)
    {
      // Failed.
      VERIFY( false );
    }

  return 0;
}


int
main()
{
  test01();
  return 0;
}
