// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation, Inc.
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

#include <complex>

// User defined type, so that the primary std::complex template is used.
namespace numext
{
  struct ldld_base
  {
    long double one;
    long double two;
  };

  struct ldld_lit : public ldld_base
  { };

  struct ldld_nonlit : public ldld_base
  {
    ~ldld_nonlit() { }
  };

  bool
  operator<(const ldld_base __a, const ldld_base __b)
  { return __a.one < __b.one && __a.two < __b.two; }

  bool
  operator==(const ldld_base __a, const ldld_base __b)
  { return __a.one == __b.one && __a.two == __b.two; }

  ldld_base
  operator+=(const ldld_base __a, const ldld_base __b)
  { return ldld_base({ __a.one + __b.one, __a.two + __b.two}); }

  ldld_base
  operator-=(const ldld_base __a, const ldld_base __b)
  { return ldld_base({ __a.one - __b.one, __a.two - __b.two}); }

  ldld_base
  operator*=(const ldld_base __a, const ldld_base __b)
  { return ldld_base({ __a.one * __b.one, __a.two * __b.two}); }

  ldld_base
  operator/=(const ldld_base __a, const ldld_base __b)
  { return ldld_base({ __a.one / __b.one, __a.two / __b.two}); }

}

constexpr std::complex<numext::ldld_lit> lit; // ok
// constexpr std::complex<numext::ldld_nonlit> nonlit; // error
