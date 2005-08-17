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
 * @file mapping_level_example.cpp
 * A basic example showing how to use different mapping levels.
 */

// For cout, endl.
#include <iostream>
// For various associative containers.
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For compound_data_type.
#include <ext/pb_assoc/data_type.hpp>
// For sting.
#include <string>
// For assert.
#include <cassert>
// For unary_function.
#include <functional>

typedef std::string user_name;

typedef unsigned long machine_id;

typedef std::string process_name;

typedef float running_time;

typedef
pb_assoc::lu_assoc_cntnr<
  process_name,
  running_time>
pr_name_rn_time_map;

typedef
pb_assoc::lu_assoc_cntnr<
  machine_id,
  pb_assoc::compound_data_type<
  pr_name_rn_time_map> >
machine_pr_map;

typedef
pb_assoc::lu_assoc_cntnr<
  user_name,
  pb_assoc::compound_data_type<
  machine_pr_map> >
user_machine_pr_map;

typedef user_machine_pr_map::rebind<2>::other l2_t;

typedef user_machine_pr_map::rebind<1>::other l1_t;

struct is_same_proc : public std::unary_function<
  l1_t::iterator::const_reference,
		      bool>
{
  is_same_proc(const process_name& r_proc) : m_proc(r_proc)
  {

  };

  bool
  operator()(l1_t::iterator::const_reference r_val)
  {
    return (r_val.first.second == m_proc);
  }

private:
  process_name m_proc;
};

int main()
{
  user_machine_pr_map m;

  /**
   * Following shows different ways to insert into a user_machine_pr_map object.
   **/

  // Josh launches on machine 1 firefox at time 1000.

  m["Josh"][1]["firefox"] = 1000;

  // Ben launches on machine 1 python at time 1003.
  ((l1_t& )m)[std::make_pair(
			     std::make_pair("Ben", 1),
			     "python")] = 1003;

  // Josh launches on machine 2 buggy at time 1004.

  m.find("Josh")->second[2]["buggy"] = 1004;

  // Sarah launches on machine 3 kdevelop at time 1010.

  m["Sarah"][2].insert(std::make_pair("kdevelop", 1010));

  // Iterate over all users running applications.

  {
    std::cout << "All users running applications:" << std::endl;

    user_machine_pr_map::const_iterator it = m.begin();

    while (it != m.end())
      std::cout << (it++)->first << std::endl;
  }

  // Sarah has terminated kdevelop
  ((l1_t& )m).erase(std::make_pair(
				   std::make_pair("Sarah", 2),
				   "kdevelop"));
}

