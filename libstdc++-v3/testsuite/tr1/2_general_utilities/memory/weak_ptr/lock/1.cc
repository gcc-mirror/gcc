// 2006-09-24  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006 Free Software Foundation
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

// TR1 2.2.4 Template class weak_ptr [tr.util.smartptr.weak]

#include <tr1/memory>
#include <testsuite_tr1.h>

// { dg-do compile }

int main()
{
  using __gnu_test::check_ret_type;
  using std::tr1::weak_ptr;
  using std::tr1::shared_ptr;

  weak_ptr<int> wp;
  check_ret_type<shared_ptr<int> >(wp.lock());
}
