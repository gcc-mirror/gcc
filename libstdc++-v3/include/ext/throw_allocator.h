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
#include <bits/functexcept.h>
#include <bits/move.h>
#ifdef __GXX_EXPERIMENTAL_CXX0X__
# include <functional>
# include <random>
#else
# include <tr1/functional>
# include <tr1/random>
#endif

_GLIBCXX_BEGIN_NAMESPACE(__gnu_cxx)

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

  // Base class for checking address and label information about
  // allocations. Create a std::map between the allocated address
  // (void*) and a datum for annotations, which are a pair of numbers
  // corresponding to label and allocated size.
  struct annotate_base
  {
    annotate_base()
    {
      label();
      map();
    }
    
    static void
    set_label(size_t l)
    { label() = l; }

    static size_t
    get_label()
    { return label(); }

    void
    insert(void* p, size_t size)
    {
      if (p == NULL)
	{
	  std::string error("throw_allocator_base::insert null insert!\n");
	  log_to_string(error, make_entry(p, size));
	  std::__throw_logic_error(error.c_str());
	}

      const_iterator found = map().find(p);
      if (found != map().end())
	{
	  std::string error("throw_allocator_base::insert double insert!\n");
	  log_to_string(error, make_entry(p, size));
	  log_to_string(error, *found);
	  std::__throw_logic_error(error.c_str());
	}

      map().insert(make_entry(p, size));
    }

    void
    erase(void* p, size_t size)
    {
      check_allocated(p, size);
      map().erase(p);
    }

    // See if a particular address and size has been allocated.
    inline void
    check_allocated(void* p, size_t size)
    {
      const_iterator found = map().find(p);
      if (found == map().end())
	{
	  std::string error("annotate_base::check_allocated by value "
			    "null erase!\n");
	  log_to_string(error, make_entry(p, size));
	  std::__throw_logic_error(error.c_str());
	}
      
      if (found->second.second != size)
	{
	  std::string error("annotate_base::check_allocated by value "
			    "wrong-size erase!\n");
	  log_to_string(error, make_entry(p, size));
	  log_to_string(error, *found);
	  std::__throw_logic_error(error.c_str());
	}
    }

    // See if a given label has been allocated.
    inline void
    check_allocated(size_t label)
    {
      const_iterator beg = map().begin();
      const_iterator end = map().end();
      std::string found;
      while (beg != end)
	{
	  if (beg->second.first == label)
	    log_to_string(found, *beg);
	  ++beg;
	}
      
      if (!found.empty())
	{
	  std::string error("annotate_base::check_allocated by label\n");
	  error += found;
	  std::__throw_logic_error(error.c_str());
	}
    }

  private:
    typedef std::pair<size_t, size_t> 		data_type;
    typedef std::map<void*, data_type> 		map_type;
    typedef map_type::value_type 		entry_type;
    typedef map_type::const_iterator 		const_iterator;
    typedef map_type::const_reference 		const_reference;

    friend std::ostream&
    operator<<(std::ostream&, const annotate_base&);

    entry_type
    make_entry(void* p, size_t size)
    { return std::make_pair(p, data_type(get_label(), size)); }

    void
    log_to_string(std::string& s, const_reference ref) const
    {
      char buf[40];
      const char tab('\t');
      s += "label: ";
      unsigned long l = static_cast<unsigned long>(ref.second.first);
      __builtin_sprintf(buf, "%lu", l);
      s += buf;
      s += tab;
      s += "size: ";
      l = static_cast<unsigned long>(ref.second.second);
      __builtin_sprintf(buf, "%lu", l);
      s += buf;
      s += tab;
      s += "address: ";
      __builtin_sprintf(buf, "%p", ref.first);
      s += buf;
      s += '\n';
    }

    static size_t&
    label()
    {
      static size_t ll;
      return ll;
    }

    static map_type&
    map()
    {
      static map_type mp;
      return mp;
    }
  };

  inline std::ostream&
  operator<<(std::ostream& os, const annotate_base& __b)
  {
    std::string error;
    typedef annotate_base base_type;
    base_type::const_iterator beg = __b.map().begin();
    base_type::const_iterator end = __b.map().end();
    for (; beg != end; ++beg)
      __b.log_to_string(error, *beg);
    return os << error;
  }

  /// Base class for probability control and throw.
  struct probability_base
  {
    // Scope-level probability adjustor objects: set probability for
    // throw at the beginning of a scope block, and restores to
    // previous probability when object is destroyed on exiting the
    // block.
    struct adjustor_base
    {
    private:
      const double _M_prob;

    public:
      adjustor_base() : _M_prob(get_probability()) { }

      virtual ~adjustor_base()
      { set_probability(_M_prob); }
    };

    // Group condition.
    struct group_adjustor : public adjustor_base
    {
      group_adjustor(size_t size)
      { set_probability(1 - std::pow(double(1 - get_probability()),
				     double(0.5 / (size + 1))));
      }
    };

    // Never enter the condition.
    struct never_adjustor : public adjustor_base
    {
      never_adjustor() { set_probability(0); }
    };

    // Always enter the condition.
    struct always_adjustor : public adjustor_base
    {
      always_adjustor() { set_probability(1); }
    };

    probability_base()
    {
      probability();
      engine();
    }

    static void
    set_probability(double __p)
    { probability() = __p; }

    static double&
    get_probability()
    { return probability(); }

    void
    throw_conditionally()
    {
      if (generate() < get_probability())
	__throw_forced_exception_error();
    }

    void
    seed(unsigned long __s)
    { engine().seed(__s); }

  private:
#ifdef __GXX_EXPERIMENTAL_CXX0X__
    typedef std::uniform_real_distribution<double> 	distribution_type;
    typedef std::mt19937 				engine_type;
#else
    typedef std::tr1::uniform_real<double> 		distribution_type;
    typedef std::tr1::mt19937 				engine_type;
#endif

    double
    generate()
    {
#ifdef __GXX_EXPERIMENTAL_CXX0X__
      const distribution_type distribution(0, 1);
      static auto generator = std::bind(distribution, engine());
#else
      // Use variate_generator to get normalized results.
      typedef std::tr1::variate_generator<engine_type, distribution_type> gen_t;
      distribution_type distribution(0, 1);
      static gen_t generator(engine(), distribution);
#endif

      double random = generator();
      if (random < distribution.min() || random > distribution.max())
	{
	  std::string __s("throw_allocator::throw_conditionally");
	  __s += "\n";
	  __s += "random number generated is: ";
	  char buf[40];
	  __builtin_sprintf(buf, "%f", random);
	  __s += buf;
	  std::__throw_out_of_range(__s.c_str());
	}

      return random;
    }

    static double&
    probability()
    {
      static double __p;
      return __p;
    }

    static engine_type&
    engine()
    {
      static engine_type __e;
      return __e;
    }
  };

  /**
   *  @brief Allocator class with logging and exception control.
   *  @ingroup allocators
   */
  template<typename T>
    class throw_allocator
    : public probability_base, public annotate_base
    {
    public:
      typedef size_t 				size_type;
      typedef ptrdiff_t 			difference_type;
      typedef T 				value_type;
      typedef value_type* 			pointer;
      typedef const value_type* 		const_pointer;
      typedef value_type& 			reference;
      typedef const value_type& 		const_reference;

    private:
      std::allocator<value_type> 		_M_allocator;

    public:
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
      { return _M_allocator.max_size(); }

      pointer
      allocate(size_type __n, std::allocator<void>::const_pointer hint = 0)
      {
	if (__n > this->max_size())
	  std::__throw_bad_alloc();

	throw_conditionally();
	pointer const a = _M_allocator.allocate(__n, hint);
	insert(a, sizeof(value_type) * __n);
	return a;
      }

      void
      construct(pointer __p, const T& val)
      { return _M_allocator.construct(__p, val); }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
      template<typename... _Args>
	void
	construct(pointer __p, _Args&&... __args)
	{ return _M_allocator.construct(__p, std::forward<_Args>(__args)...); }
#endif

      void
      destroy(pointer __p)
      { _M_allocator.destroy(__p); }

      void
      deallocate(pointer __p, size_type __n)
      {
	erase(__p, sizeof(value_type) * __n);
	_M_allocator.deallocate(__p, __n);
      }

      void
      check_allocated(pointer __p, size_type __n)
      {
	size_type __t = sizeof(value_type) * __n;
	annotate_base::check_allocated(__p, __t);
      }

      using annotate_base::check_allocated;
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
