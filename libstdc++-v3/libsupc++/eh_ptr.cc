// -*- C++ -*- Implement the members of exception_ptr.
// Copyright (C) 2008-2014 Free Software Foundation, Inc.
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
#include <bits/atomic_lockfree_defines.h>

#if ATOMIC_INT_LOCK_FREE > 1

#define _GLIBCXX_EH_PTR_COMPAT

#include <exception>
#include <bits/exception_ptr.h>
#include "unwind-cxx.h"

using namespace __cxxabiv1;

std::__exception_ptr::exception_ptr::exception_ptr() _GLIBCXX_USE_NOEXCEPT
: _M_exception_object(0) { }


std::__exception_ptr::exception_ptr::exception_ptr(void* obj)
_GLIBCXX_USE_NOEXCEPT
: _M_exception_object(obj)  { _M_addref(); }


std::__exception_ptr::exception_ptr::exception_ptr(__safe_bool)
_GLIBCXX_USE_NOEXCEPT
: _M_exception_object(0) { }


std::__exception_ptr::
exception_ptr::exception_ptr(const exception_ptr& other) _GLIBCXX_USE_NOEXCEPT
: _M_exception_object(other._M_exception_object)
{ _M_addref(); }


std::__exception_ptr::exception_ptr::~exception_ptr() _GLIBCXX_USE_NOEXCEPT
{ _M_release(); }


std::__exception_ptr::exception_ptr&
std::__exception_ptr::
exception_ptr::operator=(const exception_ptr& other) _GLIBCXX_USE_NOEXCEPT
{
  exception_ptr(other).swap(*this);
  return *this;
}


void
std::__exception_ptr::exception_ptr::_M_addref() _GLIBCXX_USE_NOEXCEPT
{
  if (_M_exception_object)
    {
      __cxa_refcounted_exception *eh =
	__get_refcounted_exception_header_from_obj (_M_exception_object);
      __atomic_add_fetch (&eh->referenceCount, 1, __ATOMIC_ACQ_REL);
    }
}


void
std::__exception_ptr::exception_ptr::_M_release() _GLIBCXX_USE_NOEXCEPT
{
  if (_M_exception_object)
    {
      __cxa_refcounted_exception *eh =
	__get_refcounted_exception_header_from_obj (_M_exception_object);
      if (__atomic_sub_fetch (&eh->referenceCount, 1, __ATOMIC_ACQ_REL) == 0)
        {
	  if (eh->exc.exceptionDestructor)
	    eh->exc.exceptionDestructor (_M_exception_object);

          __cxa_free_exception (_M_exception_object);
          _M_exception_object = 0;
        }
    }
}


void*
std::__exception_ptr::exception_ptr::_M_get() const _GLIBCXX_USE_NOEXCEPT
{ return _M_exception_object; }


void
std::__exception_ptr::exception_ptr::swap(exception_ptr &other)
  _GLIBCXX_USE_NOEXCEPT
{
  void *tmp = _M_exception_object;
  _M_exception_object = other._M_exception_object;
  other._M_exception_object = tmp;
}


// Retained for compatibility with CXXABI_1.3.
void
std::__exception_ptr::exception_ptr::_M_safe_bool_dummy()
  _GLIBCXX_USE_NOEXCEPT { }


// Retained for compatibility with CXXABI_1.3.
bool
std::__exception_ptr::exception_ptr::operator!() const _GLIBCXX_USE_NOEXCEPT
{ return _M_exception_object == 0; }


// Retained for compatibility with CXXABI_1.3.
std::__exception_ptr::exception_ptr::operator __safe_bool() const
_GLIBCXX_USE_NOEXCEPT
{
  return _M_exception_object ? &exception_ptr::_M_safe_bool_dummy : 0;
}


const std::type_info*
std::__exception_ptr::exception_ptr::__cxa_exception_type() const
  _GLIBCXX_USE_NOEXCEPT
{
  __cxa_exception *eh = __get_exception_header_from_obj (_M_exception_object);
  return eh->exceptionType;
}


bool std::__exception_ptr::operator==(const exception_ptr& lhs,
				      const exception_ptr& rhs)
  _GLIBCXX_USE_NOEXCEPT
{ return lhs._M_exception_object == rhs._M_exception_object; }


bool std::__exception_ptr::operator!=(const exception_ptr& lhs,
				      const exception_ptr& rhs)
  _GLIBCXX_USE_NOEXCEPT
{ return !(lhs == rhs);}


std::exception_ptr
std::current_exception() _GLIBCXX_USE_NOEXCEPT
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

  if (__atomic_sub_fetch (&header->referenceCount, 1, __ATOMIC_ACQ_REL) == 0)
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
  __atomic_add_fetch (&eh->referenceCount, 1,  __ATOMIC_ACQ_REL);

  dep->unexpectedHandler = get_unexpected ();
  dep->terminateHandler = get_terminate ();
  __GXX_INIT_DEPENDENT_EXCEPTION_CLASS(dep->unwindHeader.exception_class);
  dep->unwindHeader.exception_cleanup = __gxx_dependent_exception_cleanup;

#ifdef _GLIBCXX_SJLJ_EXCEPTIONS
  _Unwind_SjLj_RaiseException (&dep->unwindHeader);
#else
  _Unwind_RaiseException (&dep->unwindHeader);
#endif

  // Some sort of unwinding error.  Note that terminate is a handler.
  __cxa_begin_catch (&dep->unwindHeader);
  std::terminate();
}

#undef _GLIBCXX_EH_PTR_COMPAT

#endif
