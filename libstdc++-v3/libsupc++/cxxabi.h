// new abi support -*- C++ -*-
  
// Copyright (C) 2000, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
// 
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Written by Nathan Sidwell, Codesourcery LLC, <nathan@codesourcery.com>
 
/* This file declares the new abi entry points into the runtime. It is not
   normally necessary for user programs to include this header, or use the
   entry points directly. However, this header is available should that be
   needed.
   
   Some of the entry points are intended for both C and C++, thus this header
   is includable from both C and C++. Though the C++ specific parts are not
   available in C, naturally enough.  */

/** @file cxxabi.h
 *  The header provides an interface to the C++ ABI.
 */

#ifndef _CXXABI_H
#define _CXXABI_H 1

#pragma GCC visibility push(default)

#include <cxxabi-internal.h>
 
#ifdef __cplusplus

// User programs should use the alias `abi'. 
namespace abi = __cxxabiv1;

#endif // __cplusplus

#pragma GCC visibility pop

#endif // __CXXABI_H 
