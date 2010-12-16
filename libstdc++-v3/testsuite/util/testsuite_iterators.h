// -*- C++ -*-
// Iterator Wrappers for the C++ library testsuite. 
//
// Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
// Free Software Foundation, Inc.
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

// This file provides the following:
//
// input_iterator_wrapper, output_iterator_wrapper
// forward_iterator_wrapper, bidirectional_iterator_wrapper and
// random_access_wrapper, which attempt to exactly perform the requirements
// of these types of iterators. These are constructed from the class
// test_container, which is given two pointers to T and an iterator type.

#include <testsuite_hooks.h>
#include <bits/stl_iterator_base_types.h>

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#include <bits/move.h>
#endif

#ifndef _TESTSUITE_ITERATORS
#define _TESTSUITE_ITERATORS

#ifdef DISABLE_ITERATOR_DEBUG
#define ITERATOR_VERIFY(x)
#else
#define ITERATOR_VERIFY(x) VERIFY(x)
#endif

namespace __gnu_test
{
  /**
   * @brief Simple container for holding two pointers.
   *
   * Note that input_iterator_wrapper changes first to denote
   * how the valid range of == , ++, etc. change as the iterators are used.
   */
  template<typename T>
    struct BoundsContainer
    {
      T* first;
      T* last;
      BoundsContainer(T* _first, T* _last) : first(_first), last(_last)
      { }
    };

  // Simple container for holding state of a set of output iterators.
  template<typename T>
    struct OutputContainer : public BoundsContainer<T>
    {
      T* incrementedto;
      bool* writtento;
      OutputContainer(T* _first, T* _last)
      : BoundsContainer<T>(_first, _last), incrementedto(_first)
      {
	writtento = new bool[this->last - this->first];
	for(int i = 0; i < this->last - this->first; i++)
	  writtento[i] = false;
      }

      ~OutputContainer()
      { delete[] writtento; }
    };

  // Produced by output_iterator to allow limited writing to pointer
  template<class T>
    class WritableObject
    {
      T* ptr;

    public:
      OutputContainer<T>* SharedInfo;
      WritableObject(T* ptr_in,OutputContainer<T>* SharedInfo_in):
	ptr(ptr_in), SharedInfo(SharedInfo_in)
      { }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
      template<class U>
      void
      operator=(U&& new_val)
      {
	ITERATOR_VERIFY(SharedInfo->writtento[ptr - SharedInfo->first] == 0);
	SharedInfo->writtento[ptr - SharedInfo->first] = 1;
	*ptr = std::forward<U>(new_val);
      }
#else
      template<class U>
      void
      operator=(const U& new_val)
      {
	ITERATOR_VERIFY(SharedInfo->writtento[ptr - SharedInfo->first] == 0);
	SharedInfo->writtento[ptr - SharedInfo->first] = 1;
	*ptr = new_val;
      }
#endif
    };

  /**
   * @brief output_iterator wrapper for pointer
   * 
   * This class takes a pointer and wraps it to provide exactly
   * the requirements of a output_iterator. It should not be
   * instansiated directly, but generated from a test_container
   */
  template<class T>
  struct output_iterator_wrapper
  : public std::iterator<std::output_iterator_tag, T, std::ptrdiff_t, T*, T&>
  {
    typedef OutputContainer<T> ContainerType;
    T* ptr;
    ContainerType* SharedInfo;

    output_iterator_wrapper(T* _ptr, ContainerType* SharedInfo_in)
    : ptr(_ptr), SharedInfo(SharedInfo_in)
    {
      ITERATOR_VERIFY(ptr >= SharedInfo->first && ptr <= SharedInfo->last);
    }
    
    output_iterator_wrapper(const output_iterator_wrapper& in)
    : ptr(in.ptr), SharedInfo(in.SharedInfo)
    { }

    WritableObject<T>
    operator*() const
    {
      ITERATOR_VERIFY(ptr < SharedInfo->last);
      ITERATOR_VERIFY(SharedInfo->writtento[ptr - SharedInfo->first] == false);
      return WritableObject<T>(ptr, SharedInfo);
    }
    
    output_iterator_wrapper&
    operator=(const output_iterator_wrapper& in) 
    {
      ptr = in.ptr;
      SharedInfo = in.SharedInfo;
      return *this;
    }

    output_iterator_wrapper&
    operator++()
    {
      ITERATOR_VERIFY(SharedInfo && ptr < SharedInfo->last);
      ITERATOR_VERIFY(ptr>=SharedInfo->incrementedto);
      ptr++;
      SharedInfo->incrementedto=ptr;
      return *this;
    }

    output_iterator_wrapper
    operator++(int)
    {
      output_iterator_wrapper<T> tmp = *this;
      ++*this;
      return tmp;
    }

  };

  /**
   * @brief input_iterator wrapper for pointer
   * 
   * This class takes a pointer and wraps it to provide exactly
   * the requirements of a input_iterator. It should not be
   * instansiated directly, but generated from a test_container
   */
  template<class T>
  class input_iterator_wrapper
  : public std::iterator<std::input_iterator_tag, T, std::ptrdiff_t, T*, T&>
  {
  protected:
    input_iterator_wrapper()
    { }

  public:
    typedef BoundsContainer<T> ContainerType;
    T* ptr;
    ContainerType* SharedInfo;

    input_iterator_wrapper(T* _ptr, ContainerType* SharedInfo_in)
    : ptr(_ptr), SharedInfo(SharedInfo_in)
    { ITERATOR_VERIFY(ptr >= SharedInfo->first && ptr <= SharedInfo->last); }
    
    input_iterator_wrapper(const input_iterator_wrapper& in)
    : ptr(in.ptr), SharedInfo(in.SharedInfo)
    { }

    bool
    operator==(const input_iterator_wrapper& in) const
    {
      ITERATOR_VERIFY(SharedInfo && SharedInfo == in.SharedInfo);
      ITERATOR_VERIFY(ptr>=SharedInfo->first && in.ptr>=SharedInfo->first);
      return ptr == in.ptr;
    }

    bool
    operator!=(const input_iterator_wrapper& in) const
    {
      return !(*this == in);
    }

    T&
    operator*() const
    {
      ITERATOR_VERIFY(SharedInfo && ptr < SharedInfo->last);
      ITERATOR_VERIFY(ptr >= SharedInfo->first);
      return *ptr;
    }

    T*
    operator->() const
    {
      return &**this;
    }

    input_iterator_wrapper&
    operator=(const input_iterator_wrapper& in)
    {
      ptr = in.ptr;
      SharedInfo = in.SharedInfo;
      return *this;
    }

    input_iterator_wrapper&
    operator++()
    {
      ITERATOR_VERIFY(SharedInfo && ptr < SharedInfo->last);
      ITERATOR_VERIFY(ptr>=SharedInfo->first);
      ptr++;
      SharedInfo->first=ptr;
      return *this;
    }

    void
    operator++(int)
    {
      ++*this;
    }
  };


  /**
   * @brief forward_iterator wrapper for pointer
   * 
   * This class takes a pointer and wraps it to provide exactly
   * the requirements of a forward_iterator. It should not be
   * instansiated directly, but generated from a test_container
   */
  template<class T>
  struct forward_iterator_wrapper : public input_iterator_wrapper<T>
  {
    typedef BoundsContainer<T> ContainerType;
    typedef std::forward_iterator_tag iterator_category;
    forward_iterator_wrapper(T* _ptr, ContainerType* SharedInfo_in)
    : input_iterator_wrapper<T>(_ptr, SharedInfo_in)
    { }
    
    forward_iterator_wrapper(const forward_iterator_wrapper& in)
    : input_iterator_wrapper<T>(in)
    { }

    forward_iterator_wrapper()
    {
      this->ptr = 0;
      this->SharedInfo = 0;
    }

    T&
    operator*() const
    {
      ITERATOR_VERIFY(this->SharedInfo && this->ptr < this->SharedInfo->last);
      return *(this->ptr);
    }

    T*
    operator->() const
    { return &**this; }

    forward_iterator_wrapper&
    operator++()
    {
      ITERATOR_VERIFY(this->SharedInfo && this->ptr < this->SharedInfo->last);
      this->ptr++;
      return *this;
    }

    forward_iterator_wrapper
    operator++(int)
    {
      forward_iterator_wrapper<T> tmp = *this;
      ++*this;
      return tmp;
    }
   };

  /**
   * @brief bidirectional_iterator wrapper for pointer
   * 
   * This class takes a pointer and wraps it to provide exactly
   * the requirements of a forward_iterator. It should not be
   * instansiated directly, but generated from a test_container
   */
  template<class T>
  struct bidirectional_iterator_wrapper : public forward_iterator_wrapper<T>
  {
    typedef BoundsContainer<T> ContainerType;
    typedef std::bidirectional_iterator_tag iterator_category;
    bidirectional_iterator_wrapper(T* _ptr, ContainerType* SharedInfo_in)
    : forward_iterator_wrapper<T>(_ptr, SharedInfo_in)
    { }

    bidirectional_iterator_wrapper(const bidirectional_iterator_wrapper& in)
    : forward_iterator_wrapper<T>(in)
    { }

    bidirectional_iterator_wrapper(): forward_iterator_wrapper<T>()
    { }

    bidirectional_iterator_wrapper&
    operator=(const bidirectional_iterator_wrapper& in)
    {
      this->ptr = in.ptr;
      this->SharedInfo = in.SharedInfo;
      return *this;
    }
   
    bidirectional_iterator_wrapper&
    operator++()
    {
      ITERATOR_VERIFY(this->SharedInfo && this->ptr < this->SharedInfo->last);
      this->ptr++;
      return *this;
    }

    bidirectional_iterator_wrapper
    operator++(int)
    {
      bidirectional_iterator_wrapper<T> tmp = *this;
      ++*this;
      return tmp;
    }

    bidirectional_iterator_wrapper& 
    operator--()
    {
      ITERATOR_VERIFY(this->SharedInfo && this->ptr > this->SharedInfo->first);
      this->ptr--;
      return *this;
    }

    bidirectional_iterator_wrapper
    operator--(int)
    { 
      bidirectional_iterator_wrapper<T> tmp = *this;
      --*this;
      return tmp;
    }
   };

  /**
   * @brief random_access_iterator wrapper for pointer
   * 
   * This class takes a pointer and wraps it to provide exactly
   * the requirements of a forward_iterator. It should not be
   * instansiated directly, but generated from a test_container
   */
  template<class T>
  struct random_access_iterator_wrapper 
  : public bidirectional_iterator_wrapper<T>
  {
    typedef BoundsContainer<T> ContainerType;
    typedef std::random_access_iterator_tag iterator_category;
    random_access_iterator_wrapper(T* _ptr, ContainerType* SharedInfo_in)
    : bidirectional_iterator_wrapper<T>(_ptr, SharedInfo_in)
    { }

    random_access_iterator_wrapper(const random_access_iterator_wrapper<T>& in)
    : bidirectional_iterator_wrapper<T>(in)
    { }

    random_access_iterator_wrapper():bidirectional_iterator_wrapper<T>()
    { }

    random_access_iterator_wrapper&
    operator=(const random_access_iterator_wrapper& in)
    {
      this->ptr = in.ptr;
      this->SharedInfo = in.SharedInfo;
      return *this;
    }

    random_access_iterator_wrapper&
    operator++()
    {
      ITERATOR_VERIFY(this->SharedInfo && this->ptr < this->SharedInfo->last);
      this->ptr++;
      return *this;
    }

    random_access_iterator_wrapper
    operator++(int)
    {
      random_access_iterator_wrapper<T> tmp = *this;
      ++*this;
      return tmp;
    }

    random_access_iterator_wrapper&
    operator--()
    {
      ITERATOR_VERIFY(this->SharedInfo && this->ptr > this->SharedInfo->first);
      this->ptr--;
      return *this;
    }

    random_access_iterator_wrapper
    operator--(int)
    {
      random_access_iterator_wrapper<T> tmp = *this;
      --*this;
      return tmp;
    }

    random_access_iterator_wrapper&
    operator+=(std::ptrdiff_t n)
    {
      if(n > 0)
	{
	  ITERATOR_VERIFY(n <= this->SharedInfo->last - this->ptr);
	  this->ptr += n;
	}
      else
	{
	  ITERATOR_VERIFY(n <= this->ptr - this->SharedInfo->first);
	  this->ptr += n;
	}
      return *this;
    }

    random_access_iterator_wrapper&
    operator-=(std::ptrdiff_t n)
    { return *this += -n; }

    random_access_iterator_wrapper
    operator-(std::ptrdiff_t n) const
    {
      random_access_iterator_wrapper<T> tmp = *this;
      return tmp -= n;
    }

    std::ptrdiff_t
    operator-(const random_access_iterator_wrapper<T>& in) const
    {
      ITERATOR_VERIFY(this->SharedInfo == in.SharedInfo);
      return this->ptr - in.ptr;
    }

    T&
    operator[](std::ptrdiff_t n) const
    { return *(*this + n); }

    bool
    operator<(const random_access_iterator_wrapper<T>& in) const
    {
      ITERATOR_VERIFY(this->SharedInfo == in.SharedInfo);
      return this->ptr < in.ptr;
    }

    bool
    operator>(const random_access_iterator_wrapper<T>& in) const
    {
      return in < *this;
    }

    bool
    operator>=(const random_access_iterator_wrapper<T>& in) const
    {
      return !(*this < in);
    }

    bool 
    operator<=(const random_access_iterator_wrapper<T>& in) const
    {
      return !(*this > in);
    }
   };

  template<typename T>
    random_access_iterator_wrapper<T>
    operator+(random_access_iterator_wrapper<T> it, std::ptrdiff_t n)
    { return it += n; }

  template<typename T>
    random_access_iterator_wrapper<T>
    operator+(std::ptrdiff_t n, random_access_iterator_wrapper<T> it) 
    { return it += n; }


  /** 
   * @brief A container-type class for holding iterator wrappers
   * test_container takes two parameters, a class T and an iterator
   * wrapper templated by T (for example forward_iterator_wrapper<T>.
   * It takes two pointers representing a range and presents them as 
   * a container of iterators.
   */
  template <class T, template<class T> class ItType>
  struct test_container
  {
    typename ItType<T>::ContainerType bounds;
    test_container(T* _first, T* _last):bounds(_first, _last)
    { }

    ItType<T>
    it(int pos)
    {
      ITERATOR_VERIFY(pos >= 0 && pos <= (bounds.last - bounds.first));
      return ItType<T>(bounds.first + pos, &bounds);
    }

    ItType<T>
    it(T* pos)
    {
      ITERATOR_VERIFY(pos >= bounds.first && pos <= bounds.last);
      return ItType<T>(pos, &bounds);
    }

    ItType<T>
    begin()
    { return it(bounds.first); }

    ItType<T>
    end()
    { return it(bounds.last); }
   };
}
#endif
