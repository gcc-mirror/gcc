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
 * @file trait.hpp
 * Containsert traits for a random regression test
 *    for a specific container type.
 */

#ifndef PB_DS_REGRESSION_TEST_TRAIT_HPP
#define PB_DS_REGRESSION_TEST_TRAIT_HPP

#include <regression/trait/assoc/to_string.hpp>
#include <regression/trait/assoc/type_trait.hpp>
#include <regression/trait/assoc/native_type_trait.hpp>
#include <regression/trait/assoc/resize_trait.hpp>
#include <regression/trait/assoc/get_set_loads_trait.hpp>
#include <regression/trait/assoc/get_set_load_trait.hpp>
#include <regression/trait/assoc/node_update_trait.hpp>
#include <regression/trait/erase_if_fn.hpp>

namespace pb_ds
{
namespace test
{
namespace detail
{

#define PB_DS_CLASS_T_DEC \
  template<typename Cntnr>

#define PB_DS_CLASS_C_DEC \
  regression_test_traits<Cntnr>

#define PB_DS_TYPE_TRAITS_C_DEC \
  regression_test_type_traits<Cntnr>

#define PB_DS_NATIVE_TYPE_TRAITS_C_DEC \
  native_type_traits<typename PB_DS_TYPE_TRAITS_C_DEC::key_type, \
		     typename PB_DS_TYPE_TRAITS_C_DEC::mapped_type, \
		     typename Cntnr::allocator>

#define PB_DS_RESIZE_TRAITS_C_DEC \
  regression_test_resize_traits<Cntnr, typename Cntnr::container_category>

#define PB_DS_SET_LOADS_TRAITS_C_DEC \
  regression_test_get_set_loacontainer_traits<Cntnr,	\
					  typename Cntnr::container_category>

#define PB_DS_SET_LOAD_TRAITS_C_DEC \
  regression_test_get_set_load_traits<Cntnr,typename Cntnr::container_category>

#define PB_DS_NODE_UPDATOR_TRAITS_C_DEC \
  regression_test_node_update_traits<Cntnr, typename Cntnr::container_category>

  template<typename Cntnr>
  struct regression_test_traits : private PB_DS_TYPE_TRAITS_C_DEC,
				  private PB_DS_NATIVE_TYPE_TRAITS_C_DEC,
				  private PB_DS_RESIZE_TRAITS_C_DEC,
				  private PB_DS_NODE_UPDATOR_TRAITS_C_DEC,
				  private PB_DS_SET_LOADS_TRAITS_C_DEC,
				  private PB_DS_SET_LOAD_TRAITS_C_DEC
  {
  private:
    typedef PB_DS_NATIVE_TYPE_TRAITS_C_DEC native_type_traits_base;
    typedef PB_DS_TYPE_TRAITS_C_DEC type_traits_base;

  public:
    typedef typename Cntnr::key_type key_type;
    typedef typename Cntnr::const_key_reference const_key_reference;
    typedef typename Cntnr::value_type value_type;
    typedef typename Cntnr::const_reference const_reference;
    typedef typename PB_DS_NATIVE_TYPE_TRAITS_C_DEC::type native_type;
    typedef typename native_type::key_type native_key_type;
    typedef typename native_type::value_type native_value_type;

    enum
      {
	resize = PB_DS_RESIZE_TRAITS_C_DEC::value,
	get_set_loads = PB_DS_SET_LOADS_TRAITS_C_DEC::value,
	get_set_load = PB_DS_SET_LOAD_TRAITS_C_DEC::value,
	order_statistics = PB_DS_NODE_UPDATOR_TRAITS_C_DEC::order_statistics,
	prefix_search = PB_DS_NODE_UPDATOR_TRAITS_C_DEC::prefix_search
      };

    template<typename T>
    struct erase_if_fn : public regression_test_erase_if_fn<T>
    { };

    static size_t
    erase_if(native_type& r_native_c)
    {
      typedef regression_test_erase_if_fn<typename native_type::value_type> erase_if_fn;

      typename native_type::iterator it = r_native_c.begin();
      size_t num_ersd = 0;
      while (it != r_native_c.end())
	if (erase_if_fn()(*it))
	  {
	    ++num_ersd;
	    r_native_c.erase(it);
	    it = r_native_c.begin();
	  }
	else
	  ++it;
      return num_ersd;
    }

    static void
    print_container(const Cntnr& r_c, std::ostream& r_os)
    { PB_DS_TYPE_TRAITS_C_DEC::print_container(r_c, r_os); }

    template<typename Gen>
    static key_type
    generate_key(Gen& r_gen, size_t max)
    { return PB_DS_TYPE_TRAITS_C_DEC::generate_key(r_gen, max); }

    template<typename Gen>
    static value_type
    generate_value(Gen& r_gen, size_t max)
    { return PB_DS_TYPE_TRAITS_C_DEC::generate_value(r_gen, max); }

    static const_key_reference
    extract_key(const_reference r_val)
    { return type_traits_base::extract_key(r_val); }

    static native_key_type
    native_key(const_key_reference r_key)
    { return native_type_traits_base::native_key(r_key); }

    static native_value_type
    native_value(const_reference r_val)
    { return native_type_traits_base::native_value(r_val); }

    static const native_key_type& 
    extract_native_key(const native_value_type& r_val)
    { return native_type_traits_base::extract_key(r_val); }

    static bool
    cmp(const_reference r_val, const native_value_type& r_native_val)
    { return val_to_string(r_val) == native_val_to_string(r_native_val); }

    static std::string
    val_to_string(const_reference r_val)
    { return to_string(r_val); }

    static std::string
    key_to_string(const_key_reference r_key)
    { return to_string(r_key); }

    static std::string
    native_val_to_string(const native_value_type& r_native_val)
    { return to_string(r_native_val); }

    static bool
    prefix_match(const_key_reference r_key, const std::string& r_native_key)
    {
      const size_t native_substr_len = std::min(r_key.length(), 
						r_native_key.length());

      const std::string native_substr = r_native_key.substr(0, 
							    native_substr_len);

      return native_substr == (const std::string&) r_key;
    }
  };

#undef PB_DS_TYPE_TRAITS_C_DEC
#undef PB_DS_NATIVE_TYPE_TRAITS_C_DEC
#undef PB_DS_RESIZE_TRAITS_C_DEC
#undef PB_DS_SET_LOADS_TRAITS_C_DEC
#undef PB_DS_SET_LOAD_TRAITS_C_DEC
#undef PB_DS_NODE_UPDATOR_TRAITS_C_DEC
#undef PB_DS_CLASS_T_DEC
#undef PB_DS_CLASS_C_DEC

} // namespace detail
} // namespace test
} // namespace pb_ds

#endif
