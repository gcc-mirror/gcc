// -*- C++ -*-

// Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/** @file ext/throw_allocator.h
 *  This file is a GNU extension to the Standard C++ Library.
 *
 *  Contains an exception-throwing allocator, useful for testing
 *  exception safety. In addition, allocation addresses are stored and
 *  sanity checked.
 */

#ifndef _THROW_ALLOCATOR_H
#define _THROW_ALLOCATOR_H 1

#include <cmath>
#include <ctime>
#include <map>
#include <string>
#include <ostream>
#include <stdexcept>
#include <utility>
#include <tr1/random>
#include <bits/functexcept.h>
#include <bits/move.h>

_GLIBCXX_BEGIN_NAMESPACE(__gnu_cxx)

  class twister_rand_gen
  {    
  private:
    std::tr1::mt19937 _M_generator;

  public:
    twister_rand_gen(unsigned int seed =
		     static_cast<unsigned int>(std::time(0)))
    : _M_generator(seed) { }

    void
    init(unsigned int seed)
    { _M_generator.seed(seed); }

    double
    get_prob()
    {
      const double min = _M_generator.min();
      const double res = static_cast<const double>(_M_generator() - min);
      const double range = static_cast<const double>(_M_generator.max() - min);
      const double ret = res / range;
      _GLIBCXX_DEBUG_ASSERT(ret >= 0 && ret <= 1);
      return ret;
    }
  };

  /** 
   *  @brief Thown by throw_allocator.
   *  @ingroup exceptions
   */
  struct forced_exception_error : public std::exception
  { };

  // Substitute for concurrence_error object in the case of -fno-exceptions.
  inline void
  __throw_forced_exception_error()
  {
#if __EXCEPTIONS
    throw forced_exception_error();
#else
    __builtin_abort();
#endif
  }

  /// Base class.
  class throw_allocator_base
  {
  public:
    void
    init(unsigned long seed)
    { rand_gen().init(seed); }

    static void
    set_throw_prob(double t_p)
    { throw_prob() = t_p; }

    static double
    get_throw_prob()
    { return throw_prob(); }

    static void
    set_label(size_t l)
    { label() = l; }

    static size_t
    get_label()
    { return label(); }

    static bool
    empty()
    { return map().empty(); }

    struct group_throw_prob_adjustor
    {
      group_throw_prob_adjustor(size_t size)
      : _M_throw_prob_orig(get_throw_prob())
      {
	set_throw_prob(1 - std::pow(double(1 - get_throw_prob()),
				    double(0.5 / (size + 1))));
      }

      ~group_throw_prob_adjustor()
      { set_throw_prob(_M_throw_prob_orig); }

    private:
      const double _M_throw_prob_orig;
    };

    struct zero_throw_prob_adjustor
    {
      zero_throw_prob_adjustor()
      : _M_throw_prob_orig(get_throw_prob())
      { set_throw_prob(0); }

      ~zero_throw_prob_adjustor()
      { set_throw_prob(_M_throw_prob_orig); }

    private:
      const double _M_throw_prob_orig;
    };

  protected:
    static void
    insert(void* p, size_t size)
    {
      const_iterator found_it = map().find(p);
      if (found_it != map().end())
	{
	  std::string error("throw_allocator_base::insert double insert!\n");
	  print_to_string(error, make_entry(p, size));
	  print_to_string(error, *found_it);
	  std::__throw_logic_error(error.c_str());
	}
      map().insert(make_entry(p, size));
    }

    static void
    erase(void* p, size_t size)
    {
      check_allocated(p, size);
      map().erase(p);
    } 

    static void
    throw_conditionally()
    {
      if (rand_gen().get_prob() < get_throw_prob())
	__throw_forced_exception_error();
    }  

    // See if a particular address and size has been allocated by this
    // allocator.
    static void
    check_allocated(void* p, size_t size)
    { do_check_allocated(map().find(p), map().end(), p, size); }

    // See if a given label has been allocated by this allocator.
    static void
    check_allocated(size_t label)
    { do_check_allocated(map().begin(), map().end(), label); }

  private:
    typedef std::pair<size_t, size_t> 		alloc_data_type;
    typedef std::map<void*, alloc_data_type> 	map_type;
    typedef map_type::value_type 		entry_type;
    typedef map_type::const_iterator 		const_iterator;
    typedef map_type::const_reference 		const_reference;

    friend std::ostream& 
    operator<<(std::ostream&, const throw_allocator_base&);

    static entry_type
    make_entry(void* p, size_t size)
    { return std::make_pair(p, alloc_data_type(get_label(), size)); }

    static void
    do_check_allocated(const_iterator, const_iterator, void*, size_t);

    static void
    do_check_allocated(const_iterator, const_iterator, size_t);

    static void
    print_to_string(std::string&, const_reference);

    static map_type&
    map()
    {
      static map_type mp;
      return mp;
    }

    static twister_rand_gen&
    rand_gen()
    {
      static twister_rand_gen rg;
      return rg;
    }

    static double&
    throw_prob()
    {
      static double tp;
      return tp;
    }

    static size_t&
    label()
    {
      static size_t ll;
      return ll;
    }
  };

  inline std::ostream& 
  operator<<(std::ostream& os, const throw_allocator_base&)
  {
    std::string error;
    typedef throw_allocator_base alloc_type;
    alloc_type::const_iterator beg = alloc_type::map().begin();
    alloc_type::const_iterator end = alloc_type::map().end();
    for (; beg != end; ++beg)
      alloc_type::print_to_string(error, *beg);
    return os << error;
  }

  /** 
   *  @brief Allocator class with logging and exception control.
   *  @ingroup allocators
   */
  template<typename T>
    class throw_allocator : public throw_allocator_base
    {
    public:
      typedef size_t 				size_type;
      typedef ptrdiff_t 			difference_type;
      typedef T 				value_type;
      typedef value_type* 			pointer;
      typedef const value_type* 		const_pointer;
      typedef value_type& 			reference;
      typedef const value_type& 		const_reference;


      template<typename U>
      struct rebind
      {
        typedef throw_allocator<U> other;
      };

      throw_allocator() throw() { }

      throw_allocator(const throw_allocator&) throw() { }

      template<typename U>
      throw_allocator(const throw_allocator<U>&) throw() { }

      ~throw_allocator() throw() { }

      size_type
      max_size() const throw()
      { return std::allocator<value_type>().max_size(); }

      pointer
      allocate(size_type __n, std::allocator<void>::const_pointer hint = 0)
      {
	if (__n > this->max_size())
	  std::__throw_bad_alloc();

	throw_conditionally();
	value_type* const a = std::allocator<value_type>().allocate(__n, hint);
	insert(a, sizeof(value_type) * __n);
	return a;
      }

      void
      construct(pointer __p, const T& val)
      { return std::allocator<value_type>().construct(__p, val); }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
      template<typename... _Args>
        void
        construct(pointer __p, _Args&&... __args)
	{ 
	  return std::allocator<value_type>().
	    construct(__p, std::forward<_Args>(__args)...);
	}
#endif

      void
      destroy(pointer __p)
      { std::allocator<value_type>().destroy(__p); }

      void
      deallocate(pointer __p, size_type __n)
      {
	erase(__p, sizeof(value_type) * __n);
	std::allocator<value_type>().deallocate(__p, __n);
      }

      void
      check_allocated(pointer __p, size_type __n)
      { throw_allocator_base::check_allocated(__p, sizeof(value_type) * __n); }

      void
      check_allocated(size_type label)
      { throw_allocator_base::check_allocated(label); }
    };

  template<typename T>
    inline bool
    operator==(const throw_allocator<T>&, const throw_allocator<T>&)
    { return true; }

  template<typename T>
    inline bool
    operator!=(const throw_allocator<T>&, const throw_allocator<T>&)
    { return false; }

_GLIBCXX_END_NAMESPACE

#endif 
