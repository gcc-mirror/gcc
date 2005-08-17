// -*- C++ -*-

// Copyright (C) 2005 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice and
// this permission notice appear in supporting documentation. None of
// the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied warranty.

/**
 * @file type_utils.hpp
 * Contains utilities for handnling types. All of these classes are based on
 *	"Modern C++" by Andrei Alxandrescu.
 */

#ifndef TYPE_UTILS_HPP
#define TYPE_UTILS_HPP

#include <cstddef>
#include <utility>

namespace pb_assoc
{

  namespace detail
  {

    template<bool>
    struct static_assert;

    template<>
    struct static_assert<true>
    { };

    template<int>
    struct static_assert_dummy_class
    {
      enum
	{
	  v = 1
	};
    };

    template<class T, class U>
    class is_same_type
    {
    public:
      enum
	{
	  value = false
	};
    };

    template<class T>
    class is_same_type<
      T,
      T>
    {
    public:
      enum
	{
	  value = true
	};
    };

    template<int n>
    struct int_to_type
    {
      enum
	{
	  value = n
	};
    };

    template<typename Type>
    struct type_to_type
    {
      typedef Type type;
    };

    template<typename T>
    class unconst
    {
    private:
      template<class U>
      struct unconst_imp
      {
	typedef U type;
      };

      template<class U>
      struct unconst_imp<
	const U>
      {
	typedef U type;
      };
    public:
      typedef typename unconst_imp<T>::type type;
    };

    template<typename T>
    class unreference
    {
    private:
      template<class U>
      struct unreference_imp
      {
	typedef U type;
      };

      template<class U>
      struct unreference_imp<U&>
      {
	typedef U type;
      };
    public:
      typedef typename unreference_imp<T>::type type;
    };

    /* is_const_type
     * Idea by Andrei Alecsandrescu
     *	(Modern C++ Design: Generic Programming and Design Patterns Applied)
     **/
    template<typename T>
    class is_const_type
    {
    private:
      template<class U>
      struct is_const_type_imp
      {
	enum
	  {
	    value = 0
	  };
      };

      template<class U>
      struct is_const_type_imp<const U>
      {
	enum
	  {
	    value = 1
	  };
      };

    public:
      enum
	{
	  value = is_const_type_imp<T>::value
	};
    };

    /* is_pointer_type
    **/
    template<typename T>
    class is_pointer_type
    {
    private:
      template<class U>
      struct is_pointer_type_imp
      {
	enum
	  {
	    value = 0
	  };
      };

      template<class U>
      struct is_pointer_type_imp
      <U* >
      {
	enum
	  {
	    value = 1
	  };
      };

    public:
      enum
	{
	  value = is_pointer_type_imp<T>::value
	};
    };

    /* is_pointer_type
    **/
    template<typename T>
    class is_const_pointer_type
    {
    private:
      template<class U>
      struct is_const_pointer_type_imp
      {
	enum
	  {
	    value = 0
	  };
      };

      template<class U>
      struct is_const_pointer_type_imp
      <const U* >
      {
	enum
	  {
	    value = 1
	  };
      };

    public:
      enum
	{
	  value = is_const_pointer_type_imp<T>::value
	};
    };

    template<typename T>
    class is_reference_type
    {
    private:
      template<class U>
      struct is_reference_type_imp
      {
	enum
	  {
	    value = 0
	  };
      };

      template<class U>
      struct is_reference_type_imp<U& >
      {
	enum
	  {
	    value = 1
	  };
      };

    public:
      enum
	{
	  value = is_reference_type_imp<T>::value
	};
    };

    template<typename T>
    class is_const_reference_type
    {
    private:
      template<class U>
      struct is_const_reference_type_imp
      {
	enum
	  {
	    value = 0
	  };
      };

      template<class U>
      struct is_const_reference_type_imp<U& >
      {
	enum
	  {
	    value = 1
	  };
      };

    public:
      enum
	{
	  value = is_const_reference_type_imp<T>::value
	};
    };

    template<typename T>
    class is_member_pointer_type
    {
    private:
      template<typename U>
      struct is_member_pointer_type_imp
      {
	enum
	  {
	    value = 0
	  };
      };

      template<typename U, typename V>
      struct is_member_pointer_type_imp<
	U V::*>
      {
	enum
	  {
	    value = 1
	  };
      };

    public:
      enum
	{
	  value = is_member_pointer_type_imp<T>::value
	};
    };

#define PB_ASSOC_IS_SAME_TYPE(TYPE) is_same_type<T, TYPE>::value

    template<class T>
    class is_simple_type
    {
      template<class U>
      struct is_simple_type_imp
      {
	enum
	  {
	    value = 0
	  };
      };

      template<class U, size_t M>
      struct is_simple_type_imp<
	U[M]>
      {
	enum
	  {
	    value = is_simple_type<U>::value
	  };
      };

      template<class U>
      struct is_simple_type_imp<
	U[]>
      {
	enum
	  {
	    value = is_simple_type<U>::value
	  };
      };

      template<typename T0, typename T1>
      struct is_simple_type_imp<
	std::pair<
	T0,
	T1> >
      {
	enum
	  {
	    value = is_simple_type<T0>::value&& 
	    is_simple_type<T1>::value
	  };
      };

    public:
      enum
	{
	  value =
	  PB_ASSOC_IS_SAME_TYPE(void) ||
	  PB_ASSOC_IS_SAME_TYPE(size_t) ||
	  PB_ASSOC_IS_SAME_TYPE(const void)	||
	  PB_ASSOC_IS_SAME_TYPE(unsigned char) ||
	  PB_ASSOC_IS_SAME_TYPE(unsigned short int) ||
	  PB_ASSOC_IS_SAME_TYPE(unsigned int) ||
	  PB_ASSOC_IS_SAME_TYPE(unsigned long int) ||
	  PB_ASSOC_IS_SAME_TYPE(signed char) ||
	  PB_ASSOC_IS_SAME_TYPE(signed short int) ||
	  PB_ASSOC_IS_SAME_TYPE(int) ||
	  PB_ASSOC_IS_SAME_TYPE(long int) ||
	  PB_ASSOC_IS_SAME_TYPE(bool) ||
	  PB_ASSOC_IS_SAME_TYPE(char) ||
	  PB_ASSOC_IS_SAME_TYPE(float) ||
	  PB_ASSOC_IS_SAME_TYPE(double) ||
	  PB_ASSOC_IS_SAME_TYPE(long double) ||
	  PB_ASSOC_IS_SAME_TYPE(const unsigned char) ||
	  PB_ASSOC_IS_SAME_TYPE(const unsigned short int) ||
	  PB_ASSOC_IS_SAME_TYPE(const unsigned int) ||
	  PB_ASSOC_IS_SAME_TYPE(const unsigned long int) ||
	  PB_ASSOC_IS_SAME_TYPE(const signed char) ||
	  PB_ASSOC_IS_SAME_TYPE(const signed short int) ||
	  PB_ASSOC_IS_SAME_TYPE(const int) ||
	  PB_ASSOC_IS_SAME_TYPE(const long int) ||
	  PB_ASSOC_IS_SAME_TYPE(const bool) ||
	  PB_ASSOC_IS_SAME_TYPE(const char) ||
	  PB_ASSOC_IS_SAME_TYPE(const float) ||
	  PB_ASSOC_IS_SAME_TYPE(const double) ||
	  PB_ASSOC_IS_SAME_TYPE(const long double) ||
	  is_pointer_type<T>::value ||
	  is_const_pointer_type<T>::value ||
	  is_member_pointer_type<T>::value ||
	  is_simple_type_imp<T>::value
	};
    };

#undef PB_ASSOC_IS_SAME_TYPE

    template<bool Cond, class A, class B>
    struct cond_type;

    template<class A, class B>
    struct cond_type<
      true,
      A,
      B>
    {
      typedef A type;
    };

    template<class A, class B>
    struct cond_type<
      false,
      A,
      B>
    {
      typedef B type;
    };

  } // namespace detail

} // namespace pb_assoc

#endif // #ifndef TYPE_UTILS_HPP
