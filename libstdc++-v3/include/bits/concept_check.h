// Concept-checking control -*- C++ -*-

// Copyright (C) 2001 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#ifndef _GLIBCPP_CONCEPT_CHECK
#define _GLIBCPP_CONCEPT_CHECK 1

#pragma GCC system_header
#include <bits/c++config.h>


// Concept-checking code is on by default unless users turn it off via any
// of these methods:
//  -  _GLIBCPP_NO_CONCEPT_CHECKS is a user hook; defining it disables the
//     checks.
//  -  _STL_NO_CONCEPT_CHECKS is a user hook from the old STL implementation
//     specifically for this purpose; defining it disables the checks, in
//     case the user is expecting the old version.
//  -  NDEBUG is the usual macro that kills assert().  Defining it will also
//     disable the checks, by the reasoning that if the user doesn't want
//     any runtime assertion code, then no space penalty for the checks
//     is desired either.

// All places in libstdc++-v3 where these are used, or /might/ be used, or
// don't need to be used, or perhaps /should/ be used, are commented with
// "concept requirements" (and maybe some more text).  So grep like crazy
// if you're looking for additional places to use these.


#if defined(_GLIBCPP_NO_CONCEPT_CHECKS) || defined(_STL_NO_CONCEPT_CHECKS) \
    || defined(NDEBUG)

#define glibcpp_function_requires(...)
#define glibcpp_class_requires(a,b)
#define glibcpp_class_requires2(a,b,c)
#define glibcpp_class_requires3(a,b,c,d)
#define glibcpp_class_requires4(a,b,c,d,e)

#else // the checks are on

#include <bits/boost_concept_check.h>

// Note that the obvious and elegant approach of
//
//#define glibcpp_function_requires(C)      \
//            boost::function_requires< boost::C >()
//
// won't work due to concept templates with more than one parameter, e.g.,
// BinaryPredicateConcept.  The preprocessor tries to split things up on
// the commas in the template argument list.  We can't use an inner pair of
// parenthesis to hide the commas, because "boost::(Temp<Foo,Bar>)" isn't
// a valid instantiation pattern.

#define glibcpp_function_requires(...)                         \
            boost::function_requires< boost::__VA_ARGS__ >()
#define glibcpp_class_requires(a,C)                            \
            BOOST_CLASS_REQUIRES(a, boost, C)
#define glibcpp_class_requires2(a,b,C)                         \
            BOOST_CLASS_REQUIRES2(a, b, boost, C)
#define glibcpp_class_requires3(a,b,c,C)                       \
            BOOST_CLASS_REQUIRES3(a, b, c, boost, C)
#define glibcpp_class_requires4(a,b,c,d,C)                     \
            BOOST_CLASS_REQUIRES4(a, b, c, d, boost, C)

#endif // enable/disable

#endif // _GLIBCPP_CONCEPT_CHECK

