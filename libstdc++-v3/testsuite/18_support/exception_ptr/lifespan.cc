// { dg-do run { target c++11 } }

// 2008-05-25  Sebastian Redl  <sebastian.redl@getdesigned.at>

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

// Tests the life span of the exception object.

#include <exception>
#include <testsuite_hooks.h>

bool may_destruct = false;

class destructing
{
  mutable bool copied;

public:
  destructing() : copied(false) { }
  destructing(const destructing &o) : copied(false) { o.copied = true; }
  ~destructing() { VERIFY( copied || may_destruct ); }
};

void test01()
{
  using namespace std;

  may_destruct = false;

  // Test the destructing class.
  {
    destructing *d = new destructing;
    destructing d2(*d);
    delete d;
    may_destruct = true;
  }
  may_destruct = false;
}

void test02()
{
  using namespace std;

  may_destruct = false;

  try {
    throw destructing();
  } catch(...) {
    may_destruct = true;
  }
  may_destruct = false;
}

void test03()
{
  using namespace std;

  may_destruct = false;

  try {
    throw destructing();
  } catch(...) {
    {
      exception_ptr ep = current_exception();
    }
    may_destruct = true;
  }
  may_destruct = false;
}

void test04()
{
  using namespace std;

  may_destruct = false;

  {
    exception_ptr ep;
    try {
      throw destructing();
    } catch(...) {
      ep = current_exception();
    }
    may_destruct = true;
  }
  may_destruct = false;
}

void test05_helper()
{
  using namespace std;
  try {
    throw destructing();
  } catch(...) {
    exception_ptr ep = current_exception();
    rethrow_exception(ep);
  }
}

void test05()
{
  using namespace std;

  may_destruct = false;

  try {
    test05_helper();
  } catch(...) {
    may_destruct = true;
  }
  may_destruct = false;
}

void test06_helper()
{
  using namespace std;
  try {
    throw destructing();
  } catch(...) {
    exception_ptr ep = current_exception();
    throw;
  }
}

void test06()
{
  using namespace std;

  may_destruct = false;

  try 
    {
      test06_helper();
    } 
  catch(...) 
    {
      may_destruct = true;
    }
  may_destruct = false;
}

std::exception_ptr gep;

void test99()
{
  using namespace std;

  may_destruct = false;

  try 
    {
      throw destructing();
    } 
  catch(...) 
    {
      gep = current_exception();
    }
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();

  test99();
  may_destruct = true;
  return 0;
}
