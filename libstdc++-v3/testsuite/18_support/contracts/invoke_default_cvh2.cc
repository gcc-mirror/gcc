// Copyright (C) 2025 Free Software Foundation, Inc.
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

// { dg-options "-g0 -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe" }
// { dg-do run { target c++2a } }

#include <contracts>
#include <testsuite_hooks.h>
#include <iostream>
#include <sstream>


struct checking_buf
  : public std::streambuf
{
  bool written = false;

  checking_buf() = default;

  virtual int_type
  overflow(int_type)
  {
    written = true;
    return int_type();
  }

  std::streamsize xsputn(const char* s, std::streamsize count)
  {
    written = true;
    return count;
  }

};


bool custom_called = false;


void handle_contract_violation(const std::contracts::contract_violation& v)
{
  custom_called = true;
}




void f(int i) pre (i>10) {};

int main()
{
  checking_buf buf;
  std::cerr.rdbuf(&buf);

  f(0);
  VERIFY(!buf.written);
}

