// 981027 ncm work with libstdc++v3

// Copyright (C) 1997-2013 Free Software Foundation, Inc.
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


#include <sstream>
#include <locale>
#include <iomanip>
#include <testsuite_hooks.h>

struct MyNP : std::numpunct<char>
{
  std::string do_truename() const;
  std::string do_falsename() const;
};

std::string
MyNP::do_truename() const 
{ 
  std::string s("yea"); 
  return s; 
}

std::string
MyNP::do_falsename() const 
{ 
  std::string s("nay"); 
  return s; 
}

void
test01()
{
  bool test __attribute__((unused)) = true;
  const char lit[] = "1 0\n"
                     "true false\n"
                     ":  true:\n"
                     ":true  :\n"
                     ": false:\n"
                     ":  1:\n"
                     ":1  :\n"
                     ":  0:\n"
                     "yea nay\n"
                     ":   yea:\n"
                     ":yea   :\n"
                     ":   nay:\n";

  std::ostringstream oss;
  oss << true << " " << false << std::endl;
  oss << std::boolalpha;
  oss << true << " " << false << std::endl;

  oss << ":" << std::setw(6) << std::internal << true << ":" << std::endl;
  oss << ":" << std::setw(6) << std::left << true << ":" << std::endl;
  oss << ":" << std::setw(6) << std::right << false << ":" << std::endl;
  oss << std::noboolalpha;
  oss << ":" << std::setw(3) << std::internal << true << ":" << std::endl;
  oss << ":" << std::setw(3) << std::left << true << ":" << std::endl;
  oss << ":" << std::setw(3) << std::right << false << ":" << std::endl;

  std::locale loc = std::locale(std::locale::classic(), new MyNP);
  oss.imbue(loc);

  oss << std::boolalpha;
  oss << true << " " << false << std::endl;

  oss << ":" << std::setw(6) << std::internal << true << ":" << std::endl;
  oss << ":" << std::setw(6) << std::left << true << ":" << std::endl;
  oss << ":" << std::setw(6) << std::right << false << ":" << std::endl;

  VERIFY( oss.good() );
  VERIFY( oss.str() == lit );
}

int 
main() 
{
  test01();
  return 0;
}
