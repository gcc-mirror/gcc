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
 * @file text_populate.hpp
 * Contains a function for populating a vector with text words from
 *    a file.
 */

#ifndef PB_DS_TEXT_POPULATE_HPP
#define PB_DS_TEXT_POPULATE_HPP

#include <io/illegal_input_error.hpp>
#include <set>
#include <fstream>
#include <string>
#include <iostream>

namespace __gnu_pbds
{

  namespace test
  {

    template<typename Vec>
    void
    text_populate(const std::string& r_f_name, Vec& r_a_txt)
    {
      const size_t size = r_a_txt.size();

      try
	{
	  std::ifstream f(r_f_name.c_str());

	  if (!f.good())
	    {
	      std::cerr << "Cannot open file " << r_f_name.c_str() <<
                std::endl;

	      throw __gnu_pbds::test::illegal_input_error();
	    }

	  size_t i = 0;

	  while (f.good()&&  i < size)
	    {
	      f >> r_a_txt[i].first;
	      r_a_txt[i].second = 0;

	      ++i;
	    }

	  if (i < size)
	    {
	      std::cerr << "Read only " << static_cast<unsigned long>(i) <<
                " words" << std::endl;

	      throw __gnu_pbds::test::illegal_input_error();
	    }
	}
      catch(...)
	{
	  std::cerr << "Error reading from file" << std::endl;

	  throw;
	}
    }

    template<typename Vec>
    void
    distinct_text_populate(const std::string& r_f_name, Vec& r_a_txt)
    {
      const size_t size = r_a_txt.size();

      try
	{
	  std::ifstream f(r_f_name.c_str());

	  if (!f.good())
	    {
	      std::cerr << "Cannot open file " << r_f_name.c_str() <<
                std::endl;

	      throw __gnu_pbds::test::illegal_input_error();
	    }

	  typedef std::set< typename Vec::value_type::first_type> set_t;

	  set_t s;

	  while (f.good()&&  s.size() < size)
	    {
	      typename Vec::value_type::first_type v;

	      f >> v;

	      if (s.find(v) == s.end())
		{
		  r_a_txt[s.size()] = std::make_pair(v, 0);

		  s.insert(v);
		}
	    }

	  if (s.size() < size)
	    {
	      std::cerr << "Read only " << static_cast<unsigned long>(s.size()) <<
                " words" << std::endl;

	      throw __gnu_pbds::test::illegal_input_error();
	    }
	}
      catch(...)
	{
	  std::cerr << "Error reading from file" << std::endl;

	  throw;
	}
    }

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_TEXT_POPULATE_HPP
