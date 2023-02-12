// { dg-require-namedlocale "fr_FR.ISO8859-15" }

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

#include <locale>
#include <testsuite_hooks.h>

void test01()
{
  // This is defined through CXXFLAGS in scripts/testsuite_flags[.in].
  const char* dir = LOCALEDIR;

  std::locale l(ISO_8859(15,fr_FR));

  typedef std::messages<char> messages;

  const messages &msgs_facet = std::use_facet<messages>(l);

  messages::catalog msgs = msgs_facet.open("libstdc++", l, dir);
  VERIFY( msgs >= 0 );

  const char msgid[] = "please";
  std::string translation1 = msgs_facet.get(msgs, 0, 0, msgid);

  // Without a real translation this test doesn't mean anything:
  VERIFY( translation1 != msgid );

  // Opening another catalog was enough to show the problem, even a fake
  // catalog.
  messages::catalog fake_msgs = msgs_facet.open("fake", l);

  std::string translation2 = msgs_facet.get(msgs, 0, 0, msgid);

  // Close catalogs before doing the check to avoid leaks.
  msgs_facet.close(fake_msgs);
  msgs_facet.close(msgs);

  VERIFY( translation1 == translation2 );
}

void test02()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  // This is defined through CXXFLAGS in scripts/testsuite_flags[.in].
  const char* dir = LOCALEDIR;

  std::locale l(ISO_8859(15,fr_FR));

  typedef std::messages<wchar_t> messages;

  const messages &msgs_facet = std::use_facet<messages>(l);

  messages::catalog msgs = msgs_facet.open("libstdc++", l, dir);
  VERIFY( msgs >= 0 );

  const wchar_t msgid[] = L"please";
  std::wstring translation1 = msgs_facet.get(msgs, 0, 0, msgid);

  // Without a real translation this test doesn't mean anything:
  VERIFY( !translation1.empty() );
  VERIFY( translation1 != msgid );

  // Opening another catalog was enough to show the problem, even a fake
  // catalog.
  messages::catalog fake_msgs = msgs_facet.open("fake", l);

  std::wstring translation2 = msgs_facet.get(msgs, 0, 0, msgid);

  // Close catalogs before doing the check to avoid leaks.
  msgs_facet.close(fake_msgs);
  msgs_facet.close(msgs);

  VERIFY( translation1 == translation2 );
#endif
}

int main()
{
  test01();
  test02();
  return 0;
}
