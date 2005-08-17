// 981027 ncm work with libstdc++v3

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
// Free Software Foundation, Inc.
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
