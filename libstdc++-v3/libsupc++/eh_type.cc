// -*- C++ -*- Exception handling routines for catching.
// Copyright (C) 2001-2023 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.


#include <typeinfo>
#include <cxxabi.h>
#include "unwind-cxx.h"

namespace __cxxabiv1
{

// Returns the type_info for the currently handled exception [15.3/8], or
// null if there is none.
extern "C"
std::type_info *__cxa_current_exception_type () _GLIBCXX_NOTHROW
{
  __cxa_eh_globals *globals = __cxa_get_globals ();
  __cxa_exception *header = globals->caughtExceptions;
  if (header)
    {
      if (__is_dependent_exception (header->unwindHeader.exception_class))
        {
          __cxa_dependent_exception *de =
            __get_dependent_exception_from_ue (&header->unwindHeader);
          header = __get_exception_header_from_obj (de->primaryException);
        }
      return header->exceptionType;
    }
  else
    return 0;
}

} // namespace __cxxabiv1
