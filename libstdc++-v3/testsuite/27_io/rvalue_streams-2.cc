// { dg-do compile { target c++11 } }

// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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

struct A {};

void operator<<(std::ostream&, const A&) { }
void operator>>(std::istream&, A&) { }

class MyStream : private std::ostream, private std::istream
{
public:
  MyStream& operator <<(const char*)
  {
    return *this;
  }
  MyStream& operator >>(int&)
  {
    return *this;
  }
};

class MyStream2
{
public:
  MyStream2& operator <<(const char*)
  {
    return *this;
  }
  MyStream2& operator >>(int&)
  {
    return *this;
  }
private:
  operator std::ostream&();
  operator std::istream&();
};

struct X { };

std::ostream& operator<<(std::ostream& os, const X&) { return os; }
std::istream& operator>>(std::istream& is, X&&) { return is; }

// PR libstdc++/65543
// PR libstdc++/80940
int main()
{
  A a;

  std::ostringstream() << a;
  std::istringstream() >> a;
  MyStream stream{};
  stream << "aaa";
  int msi;
  stream >> msi;
  MyStream2 stream2{};
  stream2 << "aaa";
  stream2 >> msi;
}
