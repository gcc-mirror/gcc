// 2003-11-07  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

// Torturing by Carlo Wood.
int main()
{
  using namespace __gnu_test;

// 2003/11/07, libstdc++/12736
verify_demangle("_Z3fooIA6_KiEvA9_KT_rVPrS4_",
		"void foo<int const [6]>(int const [9][6], int restrict const (* volatile restrict) [9][6])");
// 2003/11/12, libstdc++/12947
verify_demangle("_Z1fILi5E1AEvN1CIXqugtT_Li0ELi1ELi2EEE1qE",
                "void f<5, A>(C<(((5)>(0)))?(1) : (2)>::q)");
verify_demangle("_Z1fILi5EEvN1AIXcvimlT_Li22EEE1qE",
                "void f<5>(A<(int)((5)*(22))>::q)");
verify_demangle("_Z1fPFYPFiiEiE",
                "f(int (*(*)(int))(int))");
verify_demangle("_Z1fI1XENT_1tES2_",
                "X::t f<X>(X::t)");
verify_demangle("_Z1fILi5E1AEvN1CIXstN1T1tEEXszsrS2_1tEE1qE",
                "void f<5, A>(C<sizeof (T::t), sizeof T::t>::q)");
// 2003/12/03, libstdc++/13045
verify_demangle("_Z1fILi1ELc120EEv1AIXplT_cviLd4028ae147ae147aeEEE",
                "void f<1, (char)120>(A<(1)+((int)((double)[4028ae147ae147ae]))>)");
verify_demangle("_Z1fILi1ELc120EEv1AIXplT_cviLf3f800000EEE",
                "void f<1, (char)120>(A<(1)+((int)((float)[3f800000]))>)");
verify_demangle("_Z9hairyfuncM1YKFPVPFrPA2_PM1XKFKPA3_ilEPcEiE",
                "hairyfunc(int (* const (X::** (* restrict (* volatile* (Y::*)(int) const)(char*)) [2])(long) const) [3])");

  return 0;
}
