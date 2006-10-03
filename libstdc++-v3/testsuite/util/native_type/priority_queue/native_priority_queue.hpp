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
 * @file native_priority_queue.hpp
 * Contains an adapter to Dinkumware/SGI tree tables
 */

#ifndef PB_DS_NATIVE_PRIORITY_QUEUE_HPP
#define PB_DS_NATIVE_PRIORITY_QUEUE_HPP

#include <string>
#include <vector>
#include <queue>
#include <deque>
#include <ext/pb_ds/detail/standard_policies.hpp>
#include <ext/pb_ds/detail/type_utils.hpp>
#include <io/xml.hpp>

namespace pb_ds
{
  namespace test
  {
    namespace detail
    {
      template<typename Value_Type, bool Vector, typename Allocator>
      struct base_seq
      {
      private:
	typedef typename Allocator::template rebind<Value_Type> value_rebind;

      public:
	typedef std::vector<Value_Type, typename value_rebind::other> type;
      };

      template<typename Value_Type, typename Allocator>
      struct base_seq<Value_Type, false, Allocator>
      {
      private:
	typedef typename Allocator::template rebind<Value_Type> value_rebind;

      public:
	typedef std::deque<Value_Type, typename value_rebind::other> type;
      };
    } // namespace detail

    struct native_pq_tag
    { };

#define PB_DS_CLASS_C_DEC \
    native_priority_queue<Value_Type, Vector, Cmp_Fn, Allocator>

#define PB_DS_BASE_C_DEC \
    std::priority_queue<Value_Type, typename detail::base_seq<Value_Type, Vector, Allocator>::type, Cmp_Fn>

    template<typename Value_Type,
	     bool Vector,
	     typename Cmp_Fn = std::less<Value_Type>,
	     typename Allocator = std::allocator<char> >
    class native_priority_queue : public PB_DS_BASE_C_DEC
    {
    private:
      typedef PB_DS_BASE_C_DEC base_type;
      typedef typename Allocator::template rebind<Value_Type> value_rebind;

    public:
      typedef Value_Type value_type;
      typedef typename value_rebind::other::const_reference const_reference;
      typedef native_pq_tag container_category;
      typedef Cmp_Fn cmp_fn;

      native_priority_queue() : base_type()
      { }

      template<typename It>
      native_priority_queue(It f, It l) : base_type(f, l)
      { }

      static std::string
      name()
      {
        if (Vector)
	  return ("n_pq_vector");
        return ("n_pq_deque");
      }

      static std::string
      desc()
      {
        if (Vector)
	  return make_xml_tag("type", "value", "std::priority_queue_vector");
        return make_xml_tag("type", "value", "std::priority_queue_deque");
      }

      void
      clear()
      { *static_cast<base_type*>(this) = base_type(); }

      void
      erase(const_reference r_val)
      {
        base_type tmp;
        Cmp_Fn cmp;

        while (cmp(base_type::top(), r_val) || cmp(r_val, base_type::top()))
	  {
            tmp.push(base_type::top());
            base_type::pop();
	  }

        if (!base_type::empty())
	  {
            base_type::pop();
            while (!base_type::empty())
	      {
                tmp.push(base_type::top());
                base_type::pop();
	      }
	  }
	*static_cast<base_type* >(this) = tmp;
      }

      template<typename Pred>
      size_t
      erase_if(Pred pred)
      {
        base_type tmp;
        std::size_t ersd = 0;
        while (!base_type::empty())
	  {
            if (!pred(base_type::top()))
	      tmp.push(base_type::top());
            else
	      ++ersd;
            base_type::pop();
	  }

	*static_cast<base_type*>(this) = tmp;
        return ersd;
      }

      template<typename Pred>
      void
      split(Pred pred, PB_DS_CLASS_C_DEC& other)
      {
        base_type tmp;
        other.clear();
        while (!base_type::empty())
	  {
            if (!pred(base_type::top()))
	      tmp.push(base_type::top());
            else
	      other.push(base_type::top());
            base_type::pop();
	  }
	*static_cast<base_type*>(this) = tmp;
      }

      void
      modify(const_reference r_old, const_reference r_new)
      {
        erase(r_old);
        push(r_new);
      }

      void
      join(PB_DS_CLASS_C_DEC& other)
      {
        std::vector<value_type> a_tmp;
        while (!base_type::empty())
	  {
            a_tmp.push_back(base_type::top());
            base_type::pop();
	  }

        while (!other.empty())
	  {
            a_tmp.push_back(other.top());
            other.pop();
	  }

	*static_cast<base_type*>(this) = base_type(a_tmp.begin(), a_tmp.end());
      }

      Cmp_Fn
      get_cmp_fn() const
      { return Cmp_Fn(); }
    };

#undef PB_DS_BASE_C_DEC
#undef PB_DS_CLASS_C_DEC

  } // namespace test
} // namespace pb_ds

#endif 
