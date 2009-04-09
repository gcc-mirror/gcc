// -*- C++ -*-
// Exception testing utils for the C++ library testsuite. 
//
// Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

#include <exception>
#include <testsuite_hooks.h>

#ifndef _TESTSUITE_API
#define _TESTSUITE_API 1

namespace __gnu_test
{
  // Checks for virtual public derivation in exception classes.
  // See:
  // http://www.boost.org/more/error_handling.html
  struct bad_non_virtual : virtual public std::exception { };

  template<typename Exception, bool DefaultCons>
    struct diamond_derivation_base;

  template<typename Exception>
    struct diamond_derivation_base<Exception, true>
    {
      struct diamond_derivation_error
      : bad_non_virtual, Exception
      {
	diamond_derivation_error()
        : bad_non_virtual(), Exception() { }
      };
    };

  template<typename Exception>
    struct diamond_derivation_base<Exception, false>
    {
      struct diamond_derivation_error
      : bad_non_virtual, Exception
      {
	diamond_derivation_error()
	: bad_non_virtual(), Exception("construct diamond") { }
      };
    };
  
  template<typename Exception, bool DefaultCons>
    struct diamond_derivation
    : diamond_derivation_base<Exception, DefaultCons>
    {
      typedef diamond_derivation_base<Exception, DefaultCons> base_type;
      typedef typename base_type::diamond_derivation_error error_type;

      // NB: In the libstdc++-v3 testsuite, all the standard exception
      // classes (+ a couple of extensions) are checked:  since they
      // all derive *non* virtually from std::exception, the expected
      // behavior is ambiguity.
      static void test()
      {
	bool test __attribute__((unused)) = true;      
	try
	  { throw error_type(); }
	catch (std::exception const&)
	  { VERIFY( false ); }
	catch (...) 
	  { VERIFY( true ); }
      }
    };

  // Testing type requirements for template arguments.
  struct NonDefaultConstructible
  {
    NonDefaultConstructible(int) { }
    NonDefaultConstructible(const NonDefaultConstructible&) { }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
    // For std::iota.
    NonDefaultConstructible&
    operator++()
    { return *this; }
#endif
  };
 
  // See: 20.1.1 Template argument requirements.
  inline bool
  operator==(const NonDefaultConstructible&, const NonDefaultConstructible&)
  { return false; }

  inline bool
  operator<(const NonDefaultConstructible&, const NonDefaultConstructible&)
  { return false; }

  // For 26 numeric algorithms requirements, need addable,
  // subtractable, multiplicable.
  inline NonDefaultConstructible
  operator+(const NonDefaultConstructible& lhs, 
	    const NonDefaultConstructible& rhs)
  { return NonDefaultConstructible(1); }

  inline NonDefaultConstructible
  operator-(const NonDefaultConstructible& lhs, 
	    const NonDefaultConstructible& rhs)
  { return NonDefaultConstructible(1); }

  inline NonDefaultConstructible
  operator*(const NonDefaultConstructible& lhs, 
	    const NonDefaultConstructible& rhs)
  { return NonDefaultConstructible(1); }

  // Like unary_function, but takes no argument. (ie, void).
  // Used for generator template parameter.
  template<typename _Result>
    struct void_function
    {
      typedef _Result result_type; 

      result_type
      operator()() const
      { return result_type(); }
    };

  template<>
    struct void_function<NonDefaultConstructible>
    {
      typedef NonDefaultConstructible result_type; 

      result_type
      operator()() const
      { return result_type(2); }
    };

}
#endif
