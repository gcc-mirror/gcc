// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }

// Copyright (C) 2008, 2009 Free Software Foundation
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <ratio>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;
 
  VERIFY(( std::ratio_equal<std::ratio<2,6>, std::ratio<1,3>>::value == 1 ));
  VERIFY(( std::ratio_equal<std::ratio<2,6>, std::ratio<1,4>>::value == 0 ));
  
  VERIFY( (std::ratio_not_equal<std::ratio<2,6>, 
           std::ratio<1,3>>::value == 0) );
  VERIFY( (std::ratio_not_equal<std::ratio<2,6>, 
           std::ratio<1,4>>::value == 1) );
}

void
test02()
{
  bool test __attribute__((unused)) = true;
 
  VERIFY( (std::ratio_less<std::ratio<1,4>, std::ratio<1,3>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<-1,3>, std::ratio<1,3>>::value == 1) );
  
  VERIFY( (std::ratio_less<std::ratio<1,3>, std::ratio<1,4>>::value == 0) );
  VERIFY( (std::ratio_less<std::ratio<1,3>, std::ratio<-1,3>>::value == 0) );
      
  VERIFY( (std::ratio_less_equal<std::ratio<-1,3>, 
           std::ratio<-1,3>>::value == 1) );
  VERIFY( ( std::ratio_less_equal<std::ratio<1,4>, 
           std::ratio<1,3>>::value == 1) );
  
  VERIFY( (std::ratio_less_equal<std::ratio<1,4>, 
           std::ratio<-1,3>>::value == 0) );
  VERIFY( (std::ratio_less_equal<std::ratio<1,3>, 
           std::ratio<-1,3>>::value == 0) );
  
  VERIFY( (std::ratio_greater<std::ratio<1,3>, std::ratio<1,4>>::value == 1) );
  VERIFY( (std::ratio_greater<std::ratio<1,3>, std::ratio<-1,3>>::value == 1) );
  
  VERIFY( (std::ratio_greater<std::ratio<1,4>, std::ratio<1,3>>::value == 0) );
  VERIFY( (std::ratio_greater<std::ratio<-1,3>, std::ratio<1,3>>::value == 0) );

  VERIFY( (std::ratio_greater_equal<std::ratio<1,3>, 
           std::ratio<1,3>>::value == 1) );
  VERIFY( (std::ratio_greater_equal<std::ratio<1,3>, 
           std::ratio<-1,3>>::value == 1) );

  VERIFY( (std::ratio_greater_equal<std::ratio<-1,3>, 
           std::ratio<1,3>>::value == 0) );
  VERIFY( (std::ratio_greater_equal<std::ratio<1,4>, 
           std::ratio<1,3>>::value == 0) );
}

int main()
{
  test01();
  test02();
  return 0;
}
