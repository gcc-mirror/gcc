// Copyright (C) 2009-2017 Free Software Foundation, Inc.
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

#include <typeinfo>
#include <exception>
#include "unwind.h"
#include <testsuite_hooks.h>

// Before exception_ptr was introduced, some programs copied
// part of unwind-cxx.h and used __cxa_get_globals to get at the
// current exceptionType.  __cxa_exception structure is described in the
// C++ ABI, so they have the right to assume it works.
// Ensure it is true.

struct __cxa_exception
{
  std::type_info *exceptionType;
  void (*exceptionDestructor)(void *);
  std::unexpected_handler unexpectedHandler;
  std::terminate_handler terminateHandler;
  __cxa_exception *nextException;
  int handlerCount;
#ifdef __ARM_EABI_UNWINDER__
  __cxa_exception* nextPropagatingException;
  int propagationCount;
#else
  int handlerSwitchValue;
  const unsigned char *actionRecord;
  const unsigned char *languageSpecificData;
  _Unwind_Ptr catchTemp;
  void *adjustedPtr;
#endif
  _Unwind_Exception unwindHeader;
};

struct __cxa_eh_globals
{
  __cxa_exception *caughtExceptions;
  unsigned int uncaughtExceptions;
#ifdef __ARM_EABI_UNWINDER__
  __cxa_exception* propagatingExceptions;
#endif
};

extern "C" __cxa_eh_globals *__cxa_get_globals () throw();

// PR libstdc++/38732
void test01 ()
{
  try {
    throw 0;
  } catch(...) {
    __cxa_exception *exc = __cxa_get_globals()->caughtExceptions;
    VERIFY ( exc != 0 );
    VERIFY ( typeid(int) == *exc->exceptionType );
  }
  try {
    throw 0LL;
  } catch(...) {
    __cxa_exception *exc = __cxa_get_globals()->caughtExceptions;
    VERIFY ( exc != 0 );
    VERIFY ( typeid(long long int) == *exc->exceptionType );
  }
  try {
    throw 0.0;
  } catch(...) {
    __cxa_exception *exc = __cxa_get_globals()->caughtExceptions;
    VERIFY ( exc != 0 );
    VERIFY ( typeid(double) == *exc->exceptionType );
  }
}

int main ()
{
  test01 ();
}
