// { dg-do run { target c++11 } }
// { dg-require-atomic-builtins "" }

// 2008-05-25  Sebastian Redl  <sebastian.redl@getdesigned.at>

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

// rethrow_exception() and preservation of data

#include <exception>
#include <typeinfo>
#include <cstring>
#include <stdexcept>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  try {
    rethrow_exception(make_exception_ptr(0));
  } catch(...) {
  }
}

void test02()
{
  using namespace std;

  try {
    rethrow_exception(make_exception_ptr(runtime_error("test")));
  } catch(exception &e) {
    VERIFY( typeid(e) == typeid(runtime_error) );
    VERIFY( strcmp(e.what(), "test") == 0 );
  }
}

void test03()
{
  using namespace std;

  exception_ptr ep;
  try {
    throw 0;
  } catch(...) {
    ep = current_exception();
  }
  try {
    rethrow_exception(ep);
  } catch(...) {
  }
}

void test04()
{
  using namespace std;

  // Weave the exceptions in an attempt to confuse the machinery.
  try {
    throw 0;
  } catch(...) {
    exception_ptr ep1 = current_exception();
    try {
      throw 1;
    } catch(...) {
      exception_ptr ep2 = current_exception();
      try {
        rethrow_exception(ep1);
      } catch(...) {
        try {
          rethrow_exception(ep2);
        } catch(...) {
          try {
            rethrow_exception(ep1);
          } catch(...) {
          }
          try {
            rethrow_exception(ep2);
          } catch(...) {
          }
        }
      }
    }
  }
}

void test05()
{
  // libstdc++/64651 std::rethrow_exception not found by ADL
  // This is not required to work but is a conforming extension.
  try {
    rethrow_exception(std::make_exception_ptr(0));
  } catch(...) {
  }
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();

  return 0;
}
