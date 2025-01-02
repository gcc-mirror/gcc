// 1999-10-11 bkoz

// Copyright (C) 1999-2025 Free Software Foundation, Inc.
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


// 27.5.2 template class basic_streambuf

#include <sstream>
#include <istream>
#include <testsuite_hooks.h>

// libstdc++/9318
class Outbuf : public std::streambuf
{
public:
  typedef std::streambuf::traits_type traits_type;

  std::string result() const { return str; }

protected:
  virtual int_type overflow(int_type c = traits_type::eof())
  {
    if (!traits_type::eq_int_type(c, traits_type::eof()))
      str.push_back(traits_type::to_char_type(c));
    return traits_type::not_eof(c);
  }

private:
  std::string str;
};

void test09()
{
  std::istringstream stream("Bad Moon Rising");
  Outbuf buf;
  stream >> &buf;

  VERIFY( buf.result() == "Bad Moon Rising" );
}

int main() 
{
  test09();
  return 0;
}
