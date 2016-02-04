// Copyright (C) 2013-2016 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.

// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <new>

namespace std 
{
// From N3639.  This was voted in and then back out of C++14, and is now
// just here for backward link compatibility with code built with 4.9.
class bad_array_length : public bad_alloc
{
public:
  bad_array_length() throw() { };

  // This declaration is not useless:
  // http://gcc.gnu.org/onlinedocs/gcc-3.0.2/gcc_6.html#SEC118
  virtual ~bad_array_length() throw();

  // See comment in eh_exception.cc.
  virtual const char* what() const throw();
};

bad_array_length::~bad_array_length() _GLIBCXX_USE_NOEXCEPT { }

const char*
bad_array_length::what() const _GLIBCXX_USE_NOEXCEPT
{ return "std::bad_array_length"; }

} // namespace std

namespace __cxxabiv1 {

extern "C" void
__cxa_throw_bad_array_length ()
{ _GLIBCXX_THROW_OR_ABORT(std::bad_array_length()); }

} // namespace __cxxabiv1
