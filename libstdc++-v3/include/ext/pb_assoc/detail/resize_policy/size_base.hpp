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
 * @file size_base.hpp
 * Contains an base holding size for some resize policies.
 */

#ifndef SIZE_BASE_HPP
#define SIZE_BASE_HPP

namespace pb_assoc
{

  namespace detail
  {

    template<typename Size_Type, bool Hold_Size>
    class size_base;

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	size_base< \
		Size_Type, \
		true>

    template<typename Size_Type>
    class size_base<
      Size_Type,
      true>
    {
    protected:
      typedef Size_Type size_type;

    protected:
      inline
      size_base();

      inline void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);

      inline void
      set_size(size_type size);

      inline size_type
      get_size() const;

    private:
      size_type m_size;
    };

    PB_ASSOC_CLASS_T_DEC
    PB_ASSOC_CLASS_C_DEC::
    size_base() :
      m_size(0)
    { }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    set_size(size_type size)
    {
      m_size = size;
    }

    PB_ASSOC_CLASS_T_DEC
    inline typename PB_ASSOC_CLASS_C_DEC::size_type
    PB_ASSOC_CLASS_C_DEC::
    get_size() const
    {
      return (m_size);
    }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    swap(PB_ASSOC_CLASS_C_DEC& r_other)
    {
      std::swap(m_size, r_other.m_size);
    }

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Size_Type>

#define PB_ASSOC_CLASS_C_DEC \
	size_base< \
		Size_Type, \
		false>

    template<typename Size_Type>
    class size_base<
      Size_Type,
      false>
    {
    protected:
      typedef Size_Type size_type;

    protected:
      inline void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);

      inline void
      set_size(size_type size);
    };

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    swap(PB_ASSOC_CLASS_C_DEC& /*r_other*/)
    { }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    set_size(size_type /*size*/)
    {
      // Do nothing
    }

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

  } // namespace detail

} // namespace pb_assoc

#endif // #ifndef SIZE_BASE_HPP
