// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file mem_track_allocator.hpp
 * Contains a memory-tracking allocator used for tests.
 */

#ifndef PB_DS_MEM_TRACK_ALLOCATOR_HPP
#define PB_DS_MEM_TRACK_ALLOCATOR_HPP

#include <performance/mem/mem_track_allocator_base.hpp>

namespace pb_ds
{

  namespace test
  {

#define PB_DS_CLASS_T_DEC			\
    template<typename T>

#define PB_DS_CLASS_C_DEC				\
    mem_track_allocator<				\
						T>

    template<typename T>
    class mem_track_allocator : public detail::mem_track_allocator_base
    {
    public:
      typedef size_t size_type;
      typedef ptrdiff_t difference_type;
      typedef T* pointer;
      typedef const T* const_pointer;
      typedef T& reference;
      typedef const T& const_reference;
      typedef T value_type;

      template<typename U>
      struct rebind
      {
        typedef mem_track_allocator<U> other;
      };

      mem_track_allocator() throw();

      mem_track_allocator(const PB_DS_CLASS_C_DEC& ) throw();

      template <class U>
      mem_track_allocator(const mem_track_allocator<U>& ) throw();

      ~mem_track_allocator() throw();

      size_type
      max_size() const throw();

      pointer
      allocate(size_type num, std::allocator<void>::const_pointer hint = 0);

      void
      construct(pointer p, const T& r_val);

      void
      destroy(pointer p);

      void
      deallocate(pointer p, size_type num);
    };

    PB_DS_CLASS_T_DEC
    bool
    operator==(const PB_DS_CLASS_C_DEC& , const PB_DS_CLASS_C_DEC& )
    {
      return true;
    }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    mem_track_allocator() throw()
    { }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    mem_track_allocator(const PB_DS_CLASS_C_DEC& ) throw()
    { }

    PB_DS_CLASS_T_DEC
    template<typename U>
    PB_DS_CLASS_C_DEC::
    mem_track_allocator(const mem_track_allocator<U>& ) throw()
    { }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ~mem_track_allocator() throw()
    { }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::size_type
    PB_DS_CLASS_C_DEC::
    max_size() const throw()
    {
      return (std::allocator<T>().max_size());
    }

    PB_DS_CLASS_T_DEC
    typename PB_DS_CLASS_C_DEC::pointer
    PB_DS_CLASS_C_DEC::
    allocate(size_type num, std::allocator<void>::const_pointer hint/*= 0*/)
    {
      T* const a_t = std::allocator<T>().allocate(num, hint);

      inc(sizeof(T)*  num);

      return (a_t);
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    construct(pointer p, const T& r_val)
    {
      return (std::allocator<T>().construct(p, r_val));
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    destroy(pointer p)
    {
      std::allocator<T>().destroy(p);
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    deallocate(pointer p, size_type num)
    {
      std::allocator<T>().deallocate(p, num);

      dec(sizeof(T)*  num);
    }

#undef PB_DS_CLASS_T_DEC
#undef PB_DS_CLASS_C_DEC

  } // namespace test

} // namespace pb_ds

#endif // #ifndef PB_DS_MEM_TRACK_ALLOCATOR_HPP
