// -*- C++ -*- Exception handling routines for catching.
// Copyright (C) 2001 Free Software Foundation, Inc.
//
// This file is part of GNU CC.
//
// GNU CC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// GNU CC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GNU CC; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.


#include <cstdlib>
#include "unwind-cxx.h"

using namespace __cxxabiv1;


extern "C" void *
__cxa_begin_catch (_Unwind_Exception *exceptionObject)
{
  // ??? Foreign exceptions can't be stacked here, and there doesn't
  // appear to be any place to store for __cxa_end_catch to destroy.

  __cxa_exception *header = __get_exception_header_from_ue (exceptionObject);
  __cxa_eh_globals *globals = __cxa_get_globals ();
  __cxa_exception *prev = globals->caughtExceptions;
  int count = header->handlerCount;

  if (count < 0)
    // This exception was rethrown from an immediately enclosing region.
    count = -count + 1;
  else
    count += 1;
  header->handlerCount = count;

  globals->uncaughtExceptions -= 1;
  if (header != prev)
    {
      header->nextException = prev;
      globals->caughtExceptions = header;
    }

  return header->adjustedPtr;
}


extern "C" void
__cxa_end_catch ()
{
  __cxa_eh_globals *globals = __cxa_get_globals_fast ();
  __cxa_exception *header = globals->caughtExceptions;
  int count = header->handlerCount;

  if (count < 0)
    {
      // This exception was rethrown.  Decrement the (inverted) catch
      // count and remove it from the chain when it reaches zero.
      if (++count == 0)
	{
	  globals->uncaughtExceptions += 1;
	  globals->caughtExceptions = header->nextException;
	}
    }
  else if (--count == 0)
    {
      // Handling for this exception is complete.  Destroy the object.
      globals->caughtExceptions = header->nextException;
      _Unwind_DeleteException (&header->unwindHeader);
      return;
    }
  else if (count < 0)
    // A bug in the exception handling library or compiler.
    std::abort ();

  header->handlerCount = count;
}


bool
std::uncaught_exception() throw()
{
  __cxa_eh_globals *globals = __cxa_get_globals ();
  return globals->uncaughtExceptions != 0;
}
