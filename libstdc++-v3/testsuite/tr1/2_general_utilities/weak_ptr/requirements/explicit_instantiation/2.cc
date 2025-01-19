// Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

// TR1 2.2.4 Template class weak_ptr [tr.util.smartptr.weak]

#include <tr1/memory>
#include <testsuite_tr1.h>

// { dg-do compile }

// Check the _S_single lock policy can be instantiated. For a thread-enabled
// library this checks the templates can be instantiated for non-default
// lock policy, for a single-threaded lib this is redundant but harmless.
using namespace __gnu_test;
using std::tr1::_S_single;
template class std::tr1::__weak_ptr<int, _S_single>;
template class std::tr1::__weak_ptr<void, _S_single>;
template class std::tr1::__weak_ptr<ClassType, _S_single>;
template class std::tr1::__weak_ptr<IncompleteClass, _S_single>;
