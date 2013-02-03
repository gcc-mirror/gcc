// { dg-options "-std=gnu++0x" }
// { dg-require-atomic-builtins "" }

// 2008-05-25  Sebastian Redl  <sebastian.redl@getdesigned.at>

// Copyright (C) 2008-2013 Free Software Foundation, Inc.
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
  bool test __attribute__((unused)) = true;
  using namespace std;

  try {
    rethrow_exception(copy_exception(0));
  } catch(...) {
  }
}

void test02()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  try {
    rethrow_exception(copy_exception(runtime_error("test")));
  } catch(exception &e) {
    VERIFY( typeid(e) == typeid(runtime_error) );
    VERIFY( strcmp(e.what(), "test") == 0 );
  }
}

void test03()
{
  bool test __attribute__((unused)) = true;
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
  bool test __attribute__((unused)) = true;
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

int main()
{
  test01();
  test02();
  test03();
  test04();

  return 0;
}
