// { dg-do run }
// Avoid use of non-overridable new/delete operators in shared
// { dg-options "-static" { target *-*-mingw* } }
// Test __cxa_vec routines
// Copyright (C) 2000-2021 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Apr 2000 <nathan@nathan@codesourcery.com>

#include <cxxabi.h>
#include <stdio.h>
#include <new>
#include <stdlib.h>
#include <setjmp.h>

// Allocate enough padding to hold an array cookie.
#ifdef __ARM_EABI__
static const size_t padding = 8;
#else
static const size_t padding = (sizeof (std::size_t));
#endif

// our pseudo ctors and dtors
static abi::__cxa_cdtor_return_type ctor (void *x)
{
  abort ();
}

static abi::__cxa_cdtor_return_type dtor (void *x)
{
  abort ();
}

// allocate an array whose size causes an overflow during multiplication
void test1 ()
{
  static const std::size_t large_size =
    std::size_t(1) << (sizeof(std::size_t) * 8 - 2);
  try
    {
      abi::__cxa_vec_new (large_size, 8, 0, ctor, dtor);
      abort ();
    }
  catch (std::bad_alloc &)
    {
    }
}

// allocate an array whose size causes an overflow during addition
void test2 ()
{
  try
    {
      abi::__cxa_vec_new (std::size_t(-1) / 4, 4, padding, ctor, dtor);
      abort ();
    }
  catch (std::bad_alloc &)
    {
    }
}

int main ()
{
  test1 ();
  test2 ();
}
