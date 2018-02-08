// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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


#include <mutex>
#include <thread>
#include <testsuite_hooks.h>

std::once_flag flag;
int value = 0;

struct Inc { void operator()() const { ++value; } };

struct Func
{
   void operator()() const
   {
       Inc inc;
       for (int i = 0; i < 10000;  ++i)
           std::call_once(flag, inc);
   }
};

int main()
{
   Func f;
   std::thread t1(f);
   std::thread t2(f);
   std::thread t3(f);
   t1.join();
   t2.join();
   t3.join();
   VERIFY( value == 1 );
   return 0;
}
