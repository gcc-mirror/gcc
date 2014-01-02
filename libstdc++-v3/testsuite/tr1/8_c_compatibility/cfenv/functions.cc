// { dg-do compile }

// 2006-01-26  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2014 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 8.6 Header <cfenv>

#include <tr1/cfenv>

void test01()
{
#if _GLIBCXX_USE_C99_FENV_TR1

  int except = 0, mode = 0;
  std::tr1::fexcept_t* pflag = 0;
  std::tr1::fenv_t* penv = 0;

  int ret;

  ret = std::tr1::feclearexcept(except);
  ret = std::tr1::fegetexceptflag(pflag, except);
  ret = std::tr1::feraiseexcept(except);
  ret = std::tr1::fesetexceptflag(pflag, except);
  ret = std::tr1::fetestexcept(except);

  ret = std::tr1::fegetround();
  ret = std::tr1::fesetround(mode);

  ret = std::tr1::fegetenv(penv);
  ret = std::tr1::feholdexcept(penv);
  ret = std::tr1::fesetenv(penv);
  ret = std::tr1::feupdateenv(penv);
  ret = ret; // Suppress unused warning.

#endif
}
