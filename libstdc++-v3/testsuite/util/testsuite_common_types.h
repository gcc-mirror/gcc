// -*- C++ -*-
// typelist for the C++ library testsuite. 
//
// Copyright (C) 2005 Free Software Foundation, Inc.
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

#ifndef _TESTSUITE_COMMON_TYPES_H
#define _TESTSUITE_COMMON_TYPES_H 1

#include <testsuite_visualization.h>
#include <ext/typelist.h>

#include <ext/new_allocator.h>
#include <ext/malloc_allocator.h>
#include <ext/mt_allocator.h>
#include <ext/bitmap_allocator.h>
#include <ext/pool_allocator.h>

#include <vector>
#include <list>
#include <deque>
#include <string>

#include <map>
#include <set>
#include <ext/hash_map>
#include <ext/hash_set>
#include <tr1/unordered_map>
#include <tr1/unordered_set>

namespace __gnu_test
{
  using __gnu_cxx::typelist;
  using __gnu_cxx::transform;
  using __gnu_cxx::append;

  // All the allocators to test.
  template<typename Tp, bool Thread>
    struct allocator_policies
    {
      typedef Tp			    	value_type;
      typedef __gnu_cxx::new_allocator<Tp> 		a1;
      typedef __gnu_cxx::malloc_allocator<Tp> 		a2;
      typedef __gnu_cxx::__common_pool_policy<__gnu_cxx::__pool, Thread> pool_policy;
      typedef __gnu_cxx::__mt_alloc<Tp, pool_policy>	a3;
      typedef __gnu_cxx::bitmap_allocator<Tp> 		a4;
      typedef __gnu_cxx::__pool_alloc<Tp> 		a5;
      typedef typelist<_GLIBCXX_TYPELIST_CHAIN5(a1, a2, a3, a4, a5)> type;
    };

  // Typelists for vector, string, list, deque.
  // XXX should just use template templates
  template<typename Tp, bool Thread>
    struct vectors
    {
      typedef Tp			    		value_type;

      template<typename Tl>
        struct vector_shell
	{
	  typedef Tl 					allocator_type;
	  typedef std::vector<value_type, allocator_type>	type;
	};

      typedef allocator_policies<value_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, vector_shell>::type type;
    };

  template<typename Tp, bool Thread>
    struct lists
    {
      typedef Tp			    		value_type;

      template<typename Tl>
        struct list_shell
	{
	  typedef Tl 					allocator_type;
	  typedef std::list<value_type, allocator_type>	type;
	};

      typedef allocator_policies<value_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, list_shell>::type type;
    };

  template<typename Tp, bool Thread>
    struct deques
    {
      typedef Tp			    		value_type;

      template<typename Tl>
        struct deque_shell
	{
	  typedef Tl 					allocator_type;
	  typedef std::deque<value_type, allocator_type>	type;
	};

      typedef allocator_policies<value_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, deque_shell>::type type;
    };

  template<typename Tp, bool Thread>
    struct strings
    {
      typedef Tp			    		value_type;

      template<typename Tl>
        struct string_shell
	{
	  typedef Tl 					allocator_type;
	  typedef std::char_traits<value_type> 		traits_type;
	  typedef std::basic_string<value_type, traits_type, allocator_type>	type;
	};

      typedef allocator_policies<value_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, string_shell>::type type;
    };

  // A typelist of vector, list, deque, and string all instantiated
  // with each of the allocator policies.
  template<typename Tp, bool Thread>
    struct sequence_containers
    {
      typedef Tp			    		value_type;

      typedef typename vectors<value_type, Thread>::type vector_typelist;
      typedef typename lists<value_type, Thread>::type   list_typelist;
      typedef typename deques<value_type, Thread>::type  deque_typelist;
      typedef typename strings<value_type, Thread>::type string_typelist;

      typedef typename append<vector_typelist, list_typelist>::type a1;
      typedef typename append<deque_typelist, string_typelist>::type a2;
      typedef typename append<a1, a2>::type type;
    };

  // Typelists for map, set, hash_map, hash_set, unordered_set, unordered_map.
  template<typename Tp, bool Thread>
    struct maps
    {
      typedef Tp			    		value_type;
      typedef Tp 					key_type;
      typedef std::pair<const key_type, value_type> 	pair_type;
      typedef std::less<key_type>      			compare_function;

      template<typename Tl>
        struct container
	{
	  typedef Tl 					allocator_type;
	  typedef std::map<key_type, value_type, compare_function, allocator_type>	type;
	};

      typedef allocator_policies<pair_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, container>::type type;
    };

  template<typename Tp, bool Thread>
    struct hash_maps
    {
      typedef Tp			    		value_type;
      typedef Tp 					key_type;
      typedef __gnu_cxx::hash<key_type>      		hash_function;
      typedef std::equal_to<key_type>      		equality_function;

      template<typename Tl>
        struct container
	{
	  typedef Tl 					allocator_type;
	  typedef __gnu_cxx::hash_map<key_type, value_type, hash_function, equality_function, allocator_type>	type;
	};

      typedef allocator_policies<value_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, container>::type type;
    };

  template<typename Tp, bool Thread>
    struct unordered_maps
    {
      typedef Tp			    		value_type;
      typedef Tp 					key_type;
      typedef std::pair<const key_type, value_type> 	pair_type;
      typedef std::tr1::hash<key_type>      		hash_function;
      typedef std::equal_to<key_type>      		equality_function;

      template<typename Tl>
        struct container
	{
	  typedef Tl 					allocator_type;
	  typedef std::tr1::unordered_map<key_type, value_type, hash_function, equality_function, allocator_type>	type;
	};

      typedef allocator_policies<pair_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, container>::type type;
    };

  template<typename Tp, bool Thread>
    struct sets
    {
      typedef Tp			    		value_type;
      typedef Tp 					key_type;
      typedef std::less<key_type>      			compare_function;

      template<typename Tl>
        struct container
	{
	  typedef Tl 					allocator_type;
	  typedef std::set<key_type, compare_function, allocator_type>	type;
	};

      typedef allocator_policies<key_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, container>::type type;
    };

  template<typename Tp, bool Thread>
    struct hash_sets
    {
      typedef Tp			    		value_type;
      typedef Tp 					key_type;
      typedef __gnu_cxx::hash<key_type>      		hash_function;
      typedef std::equal_to<key_type>      		equality_function;

      template<typename Tl>
        struct container
	{
	  typedef Tl 					allocator_type;
	  typedef __gnu_cxx::hash_set<key_type, hash_function, equality_function, allocator_type>	type;
	};

      typedef allocator_policies<key_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, container>::type type;
    };

  template<typename Tp, bool Thread>
    struct unordered_sets
    {
      typedef Tp			    		value_type;
      typedef Tp 					key_type;
      typedef std::tr1::hash<key_type>      		hash_function;
      typedef std::equal_to<key_type>      		equality_function;

      template<typename Tl>
        struct container
	{
	  typedef Tl 					allocator_type;
	  typedef std::tr1::unordered_set<key_type, hash_function, equality_function, allocator_type>	type;
	};

      typedef allocator_policies<key_type, Thread>	allocator_types;
      typedef typename allocator_types::type 		allocator_typelist;
      typedef typename transform<allocator_typelist, container>::type type;
    };


  // A typelist  of all associated  container types, with each  of the
  // allocator policies.
  template<typename Tp, bool Thread>
    struct associative_containers
    {
      typedef Tp			    		value_type;

      typedef typename maps<value_type, Thread>::type map_typelist;
      typedef typename sets<value_type, Thread>::type set_typelist;
      typedef typename hash_maps<value_type, Thread>::type hash_map_typelist;
      typedef typename hash_sets<value_type, Thread>::type hash_set_typelist;
      typedef typename unordered_maps<value_type, Thread>::type unordered_map_typelist;
      typedef typename unordered_sets<value_type, Thread>::type unordered_set_typelist;

      typedef typename append<map_typelist, hash_map_typelist>::type a1;
      typedef typename append<a1, unordered_map_typelist>::type a2;
      typedef typename append<set_typelist, hash_set_typelist>::type a3;
      typedef typename append<a3, unordered_set_typelist>::type a4;
      typedef typename append<a2, a4>::type type;
    };

} // namespace __gnu_test


// Function template, function objects for the tests.
template<typename TestType>
  struct value_type : public std::pair<const TestType, TestType>
  {
    inline value_type& operator++() 
    { 
      ++this->second;
      return *this; 
    }
    
    inline operator TestType() const { return this->second; }
  };

template<typename Container, int Iter>
  void
  do_loop();

template<typename Container, int Iter>
  void*
  do_thread(void* p = NULL)
  {
    do_loop<Container, Iter>();
    return p;
  }

template<typename Container, int Iter, bool Thread>
  void
  test_container(const char* filename)
  {
    using namespace __gnu_test;
    time_counter time;
    resource_counter resource;
    {
      start_counters(time, resource);
      if (!Thread)
	{
	  // No threads, so run 4x.
	  do_loop<Container, Iter * 4>();
	}
      else
	{
#if defined (_GLIBCXX_GCC_GTHR_POSIX_H) && !defined (NOTHREAD)
	  pthread_t  t1, t2, t3, t4;
	  pthread_create(&t1, 0, &do_thread<Container, Iter>, 0);
	  pthread_create(&t2, 0, &do_thread<Container, Iter>, 0);
	  pthread_create(&t3, 0, &do_thread<Container, Iter>, 0);
	  pthread_create(&t4, 0, &do_thread<Container, Iter>, 0);
	  
	  pthread_join(t1, NULL);
	  pthread_join(t2, NULL);
	  pthread_join(t3, NULL);
	  pthread_join(t4, NULL);
#endif
	}
      stop_counters(time, resource);

      // Detailed text data.
      Container obj;
      int status;
      std::ostringstream comment;
      comment << "type: " << abi::__cxa_demangle(typeid(obj).name(),
                                                 0, 0, &status);
      report_header(filename, comment.str());
      report_performance("", "", time, resource);

      // Detailed data for visualization.
      std::string vizfilename(filename);
      vizfilename += ".dat";
      write_viz_data(time, vizfilename.c_str());
    }
  }

template<bool Thread>
  struct test_sequence
  {
    test_sequence(const char* filename) : _M_filename(filename) { }

    template<class Container>
      void
      operator()(__gnu_cxx::detail::type_to_type<Container>)
      {
	const int i = 20000;
	test_container<Container, i, Thread>(_M_filename); 
      }

  private:
    const char* _M_filename;
  };


inline std::string::size_type
sequence_find_container(std::string& type)
{
  const std::string::size_type npos = std::string::npos;
  std::string::size_type n1 = type.find("vector");
  std::string::size_type n2 = type.find("list");
  std::string::size_type n3 = type.find("deque");
  std::string::size_type n4 = type.find("string");
  
  if (n1 != npos || n2 != npos || n3 != npos || n4 != npos)
    return std::min(std::min(n1, n2), std::min(n3, n4));
  else
    throw std::runtime_error("sequence_find_container not found");
}

inline std::string::size_type
associative_find_container(std::string& type)
{
  using std::string;
  string::size_type n1 = type.find("map");
  string::size_type n2 = type.find("set");
  if (n1 != string::npos || n2 != string::npos)
    return std::min(n1, n2);
  else
    throw std::runtime_error("associative_find_container not found");
}
#endif
