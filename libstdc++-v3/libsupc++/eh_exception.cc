// -*- C++ -*- std::exception implementation.
// Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
// 2003, 2004, 2005, 2006, 2007, 2009
// Free Software Foundation
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

std::exception::~exception() throw() { }

std::bad_exception::~bad_exception() throw() { }

abi::__forced_unwind::~__forced_unwind() throw() { }

abi::__foreign_exception::~__foreign_exception() throw() { }

const char* 
std::exception::what() const throw()
{
  // NB: Another elegant option would be returning typeid(*this).name()
  // and not overriding what() in bad_exception, bad_alloc, etc.  In
  // that case, however, mangled names would be returned, PR 14493.
  return "std::exception";
}

const char* 
std::bad_exception::what() const throw()
{
  return "std::bad_exception";
}
