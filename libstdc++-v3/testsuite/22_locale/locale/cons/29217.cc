// { dg-require-namedlocale "en_US.UTF-8" }

// Copyright (C) 2006-2024 Free Software Foundation, Inc.
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

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <locale>
#include <testsuite_hooks.h>

// libstdc++/29217
void test01()
{
  using namespace std;

  locale::global(locale(locale(), "en_US.UTF-8",
			locale::collate | locale::ctype));

  VERIFY( locale().name() == "LC_CTYPE=en_US.UTF-8;LC_NUMERIC=C;"
	  "LC_TIME=C;LC_COLLATE=en_US.UTF-8;LC_MONETARY=C;LC_MESSAGES=C;"
	  "LC_PAPER=C;LC_NAME=C;LC_ADDRESS=C;LC_TELEPHONE=C;"
	  "LC_MEASUREMENT=C;LC_IDENTIFICATION=C" );

  VERIFY( locale().name() == setlocale(LC_ALL, 0) );

  locale loc1 = locale(locale::classic(), "en_US.UTF-8", locale::time);

  VERIFY( loc1.name() == "LC_CTYPE=C;LC_NUMERIC=C;LC_TIME=en_US.UTF-8;"
	  "LC_COLLATE=C;LC_MONETARY=C;LC_MESSAGES=C;LC_PAPER=C;LC_NAME=C;"
	  "LC_ADDRESS=C;LC_TELEPHONE=C;LC_MEASUREMENT=C;"
	  "LC_IDENTIFICATION=C" );
}

int main()
{
  test01();
  return 0;
}
