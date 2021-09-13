// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

// IA 64 C++ ABI - 5.1 External Names (a.k.a. Mangling)

#include <testsuite_hooks.h>

// libcwd tests
int main()
{
  using namespace __gnu_test;

/*
struct option { };
namespace std {
  template<typename T1, typename T2>
    class __normal_iterator {
    public:
      void operator-(__normal_iterator const&) const { }
    };
}

void k(void)
{
  // Instantiation.
  std::__normal_iterator<option const*, std::vector<option> > dummy;
  dummy.operator-(dummy);
}
*/
  verify_demangle("_ZNKSt17__normal_iteratorIPK6optionSt6vectorIS0_SaIS0_EEEmiERKS6_",
       "std::__normal_iterator<option const*, std::vector<option, std::allocator<option> > >::operator-(std::__normal_iterator<option const*, std::vector<option, std::allocator<option> > > const&) const");

  return 0;
}
