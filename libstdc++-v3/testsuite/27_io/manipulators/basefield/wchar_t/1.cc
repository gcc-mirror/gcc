// Copyright (C) 2004 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <sstream>
#include <locale>
#include <iomanip>
#include <testsuite_hooks.h>
			
struct MyNP : std::numpunct<wchar_t>
{
  std::string do_grouping() const;
  wchar_t do_thousands_sep() const;
};

std::string
MyNP::do_grouping() const
{
  std::string s("\3");
  return s;
}

wchar_t
MyNP::do_thousands_sep() const
{ return L' '; }

void test01()
{
  bool test __attribute__((unused)) = true;
  const wchar_t lit[] = L"0123 456\n"
                        L": 01 234 567:\n"
                        L":0123 456   :\n"
                        L":    012 345:\n"
                        L":     01 234:\n"
                        L":0726 746 425:\n"
                        L":04 553 207 :\n"
                        L":   0361 100:\n"
                        L":       0173:\n"
                        L"0x12 345 678\n"
                        L"|0x000012 345 678|\n"
                        L"|0x12 345 6780000|\n"
                        L"|00000x12 345 678|\n"
                        L"|0x000012 345 678|\n";

  std::wostringstream oss;
  oss.imbue(std::locale(std::locale(), new MyNP));

  // Octals
  oss << std::oct << std::showbase;
  oss << 0123456l << std::endl;

  oss << L":" << std::setw(11);
  oss << 01234567l << L":" << std::endl;

  oss << L":" << std::setw(11) << std::left;
  oss << 0123456l << L":" << std::endl;

  oss << L":" << std::setw(11) << std::right;
  oss << 012345l << L":" << std::endl;

  oss << L":" << std::setw(11) << std::internal;
  oss << 01234l << L":" << std::endl;

  oss << L":" << std::setw(11);
  oss << 123456789l << L":" << std::endl;

  oss << L":" << std::setw(11) << std::left;
  oss << 1234567l << L":" << std::endl;

  oss << L":" << std::setw(11) << std::right;
  oss << 123456l << L":" << std::endl;

  oss << L":" << std::setw(11) << std::internal;
  oss << 123l << L":" << std::endl;

  // Hexadecimals
  oss << std::hex << std::setfill(L'0');
  oss << 0x12345678l << std::endl;

  oss << L"|" << std::setw(16);
  oss << 0x12345678l << L"|" << std::endl;

  oss << L"|" << std::setw(16) << std::left;
  oss << 0x12345678l << L"|" << std::endl;

  oss << L"|" << std::setw(16) << std::right;
  oss << 0x12345678l << L"|" << std::endl;

  oss << L"|" << std::setw(16) << std::internal;
  oss << 0x12345678l << L"|" << std::endl;

  VERIFY( oss.good() );
  VERIFY( oss.str() == lit );
}

int 
main() 
{
  test01();
  return 0;
}
