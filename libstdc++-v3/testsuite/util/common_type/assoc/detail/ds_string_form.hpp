// -*- C++ -*-

// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
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

/**
 * @file ds_string_form.hpp
 * Transforms containers into string form.
 */

#ifndef PB_DS_DS_STRING_FORM_HPP
#define PB_DS_DS_STRING_FORM_HPP

#include <string>
#include <ext/pb_ds/tag_and_trait.hpp>
#include <common_type/assoc/detail/list_update_policy_string_form.hpp>
#include <common_type/assoc/detail/comb_hash_fn_string_form.hpp>
#include <common_type/assoc/detail/resize_policy_string_form.hpp>
#include <common_type/assoc/detail/probe_fn_string_form.hpp>
#include <common_type/assoc/detail/tree_supports_order_statistics.hpp>
#include <common_type/assoc/detail/trie_supports_order_statistics.hpp>
#include <common_type/assoc/detail/trie_supports_prefix_search.hpp>
#include <common_type/assoc/detail/store_hash_string_form.hpp>
#include <io/xml.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      template<typename Cntnr, class Tag>
      struct ds_string_form;

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::cc_hash_tag>
      {
	static std::string
        name()
	{
	  return ("cc_hash_" +
		  comb_hash_fn_string_form<typename Cntnr::comb_hash_fn>::name() +
		  resize_policy_string_form<typename Cntnr::resize_policy>::name() +
		  store_hash_string_form<Cntnr::store_hash>::name());
	}

	static std::string
        desc()
	{
	  const std::string comb_hash_fn_desc =
            comb_hash_fn_string_form<typename Cntnr::comb_hash_fn>::desc();

	  const std::string resize_policy_desc =
            resize_policy_string_form<typename Cntnr::resize_policy>::desc();

	  const std::string store_hash_desc =
            store_hash_string_form<Cntnr::store_hash>::desc();

	  return (make_xml_tag("type", "value", "cc_hash_table", comb_hash_fn_desc + resize_policy_desc + store_hash_desc));
	}
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::gp_hash_tag>
      {
	static std::string
        name()
	{
	  return ("gp_hash_" +
		  comb_hash_fn_string_form<typename Cntnr::comb_probe_fn>::name() +
		  probe_fn_string_form<typename Cntnr::probe_fn>::name() +
		  resize_policy_string_form<typename Cntnr::resize_policy>::name() +
		  store_hash_string_form<Cntnr::store_hash>::name());
	}

	static std::string
        desc()
	{
	  const std::string comb_probe_fn_desc =
            comb_hash_fn_string_form<typename Cntnr::comb_probe_fn>::desc();

	  const std::string probe_fn_desc =
            probe_fn_string_form<typename Cntnr::probe_fn>::desc();

	  const std::string resize_policy_desc =
            resize_policy_string_form<typename Cntnr::resize_policy>::desc();

	  const std::string store_hash_desc =
            store_hash_string_form<Cntnr::store_hash>::desc();

	  return make_xml_tag("type", "value", "gp_hash_table",
			      comb_probe_fn_desc + probe_fn_desc + resize_policy_desc + store_hash_desc);
	}
      };

      template<typename Cntnr>
      struct tree_ds_string_form
      {
	static std::string
        name(const std::string container_category_str)
	{
	  if (tree_supports_order_statistics<Cntnr>::value)
            return (container_category_str + "ost_");
	  return container_category_str;
	}

	static std::string
        desc(const std::string container_category_str)
	{
	  const std::string category_str =
            make_xml_tag("Tag",  "value",  container_category_str);

	  const std::string node_update_str =
            make_xml_tag("Node_Update",
			 "value",(tree_supports_order_statistics<Cntnr>::value ?
	 "tree_order_statistics_node_update" : "null_node_update"));

	  return make_xml_tag("type", "value", "tree", category_str + node_update_str);
	}
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::rb_tree_tag>
      : private tree_ds_string_form<Cntnr>
      {
      private:
	typedef tree_ds_string_form< Cntnr> base_type;

      public:
	static std::string
        name()
	{ return base_type::name("rb_tree_"); }

	static std::string
        desc()
	{ return base_type::desc("rb_tree_tag"); }
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::splay_tree_tag>
      : private tree_ds_string_form<Cntnr>
      {
      private:
	typedef tree_ds_string_form< Cntnr> base_type;

      public:
	static std::string
        name()
	{ return base_type::name("splay_tree_"); }

	static std::string
        desc()
	{ return base_type::desc("splay_tree_tag"); }
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::ov_tree_tag>
      : private tree_ds_string_form<Cntnr>
      {
      private:
	typedef tree_ds_string_form< Cntnr> base_type;

      public:
	static std::string
        name()
	{ return (base_type::name("ov_tree_")); }

	static std::string
        desc()
	{ return (base_type::desc("ov_tree_tag")); }
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::list_update_tag>
      {
	static std::string
        name()
	{
	  return ("lu_" +
		  lu_policy_string_form<typename Cntnr::update_policy>::name());
	}

	static std::string
        desc()
	{
	  return make_xml_tag("type", "value", "list_update",
			       lu_policy_string_form<typename Cntnr::update_policy>::desc());
	}
      };

      template<typename Cntnr>
      struct ds_string_form<Cntnr, __gnu_pbds::pat_trie_tag>
      {
	static std::string
        name()
	{
	  if (trie_supports_order_statistics<Cntnr>::value)
            return ("pat_trie_ost_");

	  if (trie_supports_prefix_search<Cntnr>::value)
            return ("pat_trie_prs_");

	  return ("pat_trie_");
	}

	static std::string
        desc()
	{
	  std::string category_s = make_xml_tag("Tag", "value", "pat_trie_tag");
	  const char* s;
	  if (trie_supports_order_statistics<Cntnr>::value)
            s = "trie_order_statistics_node_update";
	  else if (trie_supports_prefix_search<Cntnr>::value)
	    s = "trie_prefix_search_node_update";
	  else
            s = "null_node_update";
	  std::string node_s = make_xml_tag("Node_Update", "value", s);
	  return make_xml_tag("type", "value", "trie", category_s + node_s);
	}
      };

    } // namespace detail

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_DS_STRING_FORM_HPP

