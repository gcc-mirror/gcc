// -*- C++ -*-
// Exception testing utils for the C++ library testsuite. 
//
// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <testsuite_hooks.h>

#ifndef _TESTSUITE_EH
#define _TESTSUITE_EH 1

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
      struct diamond_derivation_error: bad_non_virtual, Exception
      {
	diamond_derivation_error() : bad_non_virtual(), Exception() { }
      };
    };

  template<typename Exception>
    struct diamond_derivation_base<Exception, false>
    {
      struct diamond_derivation_error: bad_non_virtual, Exception
      {
	diamond_derivation_error()
	: bad_non_virtual(), Exception("construct diamond") { }
      };
    };
  
  template<typename Exception, bool DefaultCons>
    struct diamond_derivation: diamond_derivation_base<Exception, DefaultCons>
    {
      typedef diamond_derivation_base<Exception, DefaultCons> base_type;
      typedef typename base_type::diamond_derivation_error error_type;
      
      static void test()
      {
	bool test __attribute__((unused)) = true;      
	try { throw error_type(); }
	catch (std::exception const& e) { }
	catch (...) 
	  { VERIFY( false ); }
      }
    };
}
#endif
