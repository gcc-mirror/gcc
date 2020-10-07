// -*- C++ -*- Implement the members of exception_ptr.
// Copyright (C) 2008-2020 Free Software Foundation, Inc.
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

#include <bits/c++config.h>
#include "eh_atomics.h"

#define _GLIBCXX_EH_PTR_COMPAT

#include <exception>
#include <bits/exception_ptr.h>
#include "unwind-cxx.h"

using namespace __cxxabiv1;

// Verify assumptions about member layout in exception types
namespace
{
template<typename Ex>
  constexpr std::size_t unwindhdr()
  { return offsetof(Ex, unwindHeader); }

template<typename Ex>
  constexpr std::size_t termHandler()
  { return unwindhdr<Ex>() - offsetof(Ex, terminateHandler); }

static_assert( termHandler<__cxa_exception>()
	       == termHandler<__cxa_dependent_exception>(),
	       "__cxa_dependent_exception::termHandler layout must be"
	       " consistent with __cxa_exception::termHandler" );

#ifndef __ARM_EABI_UNWINDER__
template<typename Ex>
  constexpr std::ptrdiff_t adjptr()
  { return unwindhdr<Ex>() - offsetof(Ex, adjustedPtr); }

static_assert( adjptr<__cxa_exception>()
	       == adjptr<__cxa_dependent_exception>(),
	       "__cxa_dependent_exception::adjustedPtr layout must be"
	       " consistent with __cxa_exception::adjustedPtr" );
#endif
}

std::__exception_ptr::exception_ptr::exception_ptr(void* obj) noexcept
: _M_exception_object(obj)  { _M_addref(); }


std::__exception_ptr::exception_ptr::exception_ptr(__safe_bool) noexcept
: _M_exception_object(nullptr) { }


void
std::__exception_ptr::exception_ptr::_M_addref() noexcept
{
  if (__builtin_expect(_M_exception_object != nullptr, true))
    {
      __cxa_refcounted_exception *eh =
	__get_refcounted_exception_header_from_obj (_M_exception_object);
      __gnu_cxx::__eh_atomic_inc (&eh->referenceCount);
    }
}


void
std::__exception_ptr::exception_ptr::_M_release() noexcept
{
  if (__builtin_expect(_M_exception_object != nullptr, true))
    {
      __cxa_refcounted_exception *eh =
	__get_refcounted_exception_header_from_obj (_M_exception_object);
      if (__gnu_cxx::__eh_atomic_dec (&eh->referenceCount))
        {
	  if (eh->exc.exceptionDestructor)
	    eh->exc.exceptionDestructor (_M_exception_object);

          __cxa_free_exception (_M_exception_object);
          _M_exception_object = nullptr;
        }
    }
}


void*
std::__exception_ptr::exception_ptr::_M_get() const noexcept
{ return _M_exception_object; }



// Retained for compatibility with CXXABI_1.3.
void
std::__exception_ptr::exception_ptr::_M_safe_bool_dummy() noexcept { }


// Retained for compatibility with CXXABI_1.3.
bool
std::__exception_ptr::exception_ptr::operator!() const noexcept
{ return _M_exception_object == nullptr; }


// Retained for compatibility with CXXABI_1.3.
std::__exception_ptr::exception_ptr::operator __safe_bool() const noexcept
{
  return _M_exception_object ? &exception_ptr::_M_safe_bool_dummy : nullptr;
}

const std::type_info*
std::__exception_ptr::exception_ptr::__cxa_exception_type() const noexcept
{
  __cxa_exception *eh = __get_exception_header_from_obj (_M_exception_object);
  return eh->exceptionType;
}

// Retained for compatibility with CXXABI_1.3.12.
bool
std::__exception_ptr::operator==(const exception_ptr& lhs,
				 const exception_ptr& rhs) noexcept
{ return lhs._M_exception_object == rhs._M_exception_object; }

// Retained for compatibility with CXXABI_1.3.12.
bool
std::__exception_ptr::operator!=(const exception_ptr& lhs,
				 const exception_ptr& rhs) noexcept
{ return !(lhs == rhs); }


std::exception_ptr
std::current_exception() noexcept
{
  __cxa_eh_globals *globals = __cxa_get_globals ();
  __cxa_exception *header = globals->caughtExceptions;

  if (!header)
    return std::exception_ptr();

  // Since foreign exceptions can't be counted, we can't return them.
  if (!__is_gxx_exception_class (header->unwindHeader.exception_class))
    return std::exception_ptr();

  return std::exception_ptr(
    __get_object_from_ambiguous_exception (header));
}


static void
__gxx_dependent_exception_cleanup(_Unwind_Reason_Code code,
				  _Unwind_Exception *exc)
{
  // This cleanup is set only for dependents.
  __cxa_dependent_exception *dep = __get_dependent_exception_from_ue (exc);
  __cxa_refcounted_exception *header =
    __get_refcounted_exception_header_from_obj (dep->primaryException);

  // We only want to be called through _Unwind_DeleteException.
  // _Unwind_DeleteException in the HP-UX IA64 libunwind library
  // returns _URC_NO_REASON and not _URC_FOREIGN_EXCEPTION_CAUGHT
  // like the GCC _Unwind_DeleteException function does.
  if (code != _URC_FOREIGN_EXCEPTION_CAUGHT && code != _URC_NO_REASON)
    __terminate (header->exc.terminateHandler);

  __cxa_free_dependent_exception (dep);

  if (__gnu_cxx::__eh_atomic_dec (&header->referenceCount))
    {
      if (header->exc.exceptionDestructor)
	header->exc.exceptionDestructor (header + 1);

      __cxa_free_exception (header + 1);
    }
}


void
std::rethrow_exception(std::exception_ptr ep)
{
  void *obj = ep._M_get();
  __cxa_refcounted_exception *eh
    = __get_refcounted_exception_header_from_obj (obj);

  __cxa_dependent_exception *dep = __cxa_allocate_dependent_exception ();
  dep->primaryException = obj;
  __gnu_cxx::__eh_atomic_inc (&eh->referenceCount);

  dep->unexpectedHandler = get_unexpected ();
  dep->terminateHandler = get_terminate ();
  __GXX_INIT_DEPENDENT_EXCEPTION_CLASS(dep->unwindHeader.exception_class);
  dep->unwindHeader.exception_cleanup = __gxx_dependent_exception_cleanup;

  __cxa_eh_globals *globals = __cxa_get_globals ();
  globals->uncaughtExceptions += 1;

#ifdef __USING_SJLJ_EXCEPTIONS__
  _Unwind_SjLj_RaiseException (&dep->unwindHeader);
#else
  _Unwind_RaiseException (&dep->unwindHeader);
#endif

  // Some sort of unwinding error.  Note that terminate is a handler.
  __cxa_begin_catch (&dep->unwindHeader);
  std::terminate();
}

#undef _GLIBCXX_EH_PTR_COMPAT
