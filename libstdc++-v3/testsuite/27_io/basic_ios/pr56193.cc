// { dg-do compile }
// { dg-options "-std=gnu++11" }
// Copyright (C) 2013 Free Software Foundation, Inc.

#include <iostream>

// PR libstdc++/56193

int
test01()
{
  std::cout << std::cout; // { dg-error "cannot bind" }
}

// { dg-error "initializing argument" "" { target *-*-* } 602 }
