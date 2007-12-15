// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation
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

// 20.6.6.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <testsuite_tr1.h>

// Check the _S_single lock policy can be instantiated. For a thread-enabled
// library this checks the templates can be instantiated for non-default
// lock policy, for a single-threaded lib this is redundant but harmless.
using namespace __gnu_test;
using std::__weak_ptr;
using std::_S_single;
template class __weak_ptr<int, _S_single>;
template class __weak_ptr<void, _S_single>;
template class __weak_ptr<ClassType, _S_single>;
template class __weak_ptr<IncompleteClass, _S_single>;
