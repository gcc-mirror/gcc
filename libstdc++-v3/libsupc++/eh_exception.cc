// -*- C++ -*- std::exception implementation.
// Copyright (C) 1994-2020 Free Software Foundation, Inc.
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

#include "typeinfo"
#include "exception"
#include <cxxabi.h>

std::exception::~exception() _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_USE_NOEXCEPT { }

std::bad_exception::~bad_exception() _GLIBCXX_TXN_SAFE_DYN
    _GLIBCXX_USE_NOEXCEPT
{ }

abi::__forced_unwind::~__forced_unwind() throw() { }

abi::__foreign_exception::~__foreign_exception() throw() { }

const char* 
std::exception::what() const _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_USE_NOEXCEPT
{
  // NB: Another elegant option would be returning typeid(*this).name()
  // and not overriding what() in bad_exception, bad_alloc, etc.  In
  // that case, however, mangled names would be returned, PR 14493.
  return "std::exception";
}

const char* 
std::bad_exception::what() const _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_USE_NOEXCEPT
{
  return "std::bad_exception";
}

// Transactional clones for the destructors and what().
// what() is effectively transaction_pure, but we do not want to annotate it
// as such; thus, we call exactly the respective nontransactional function.
extern "C" {

void
_ZGTtNKSt9exceptionD1Ev(const std::exception*)
{ }

const char*
_ZGTtNKSt9exception4whatEv(const std::exception* that)
{
  // We really want the non-virtual call here.  We already executed the
  // indirect call representing the virtual call, and the TM runtime or the
  // compiler resolved it to this transactional clone.  In the clone, we want
  // to do the same as for the nontransactional original, so we just call it.
  return that->std::exception::what();
}

void
_ZGTtNKSt13bad_exceptionD1Ev(
    const std::bad_exception*)
{ }

const char*
_ZGTtNKSt13bad_exception4whatEv(
    const std::bad_exception* that)
{
  // Also see _ZGTtNKSt9exception4whatEv.
  return that->std::bad_exception::what();
}

}
