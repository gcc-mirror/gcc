// Throw Allocator. Out of line function definitions. -*- C++ -*-

// Copyright (C) 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <ext/throw_allocator.h>

_GLIBCXX_BEGIN_NAMESPACE(__gnu_cxx)

  void
  throw_allocator_base::do_check_allocated(const_iterator found,
					   const_iterator end,
					   void* p, size_t size)
  {
    if (found == end)
      {
	std::string error("throw_allocator_base::check_allocated by value "
			  "null erase!\n");
	print_to_string(error, make_entry(p, size));
	std::__throw_logic_error(error.c_str());
      }

    if (found->second.second != size)
      {
	std::string error("throw_allocator_base::check_allocated by value "
			  "wrong-size erase!\n");
	print_to_string(error, make_entry(p, size));
	print_to_string(error, *found);
	std::__throw_logic_error(error.c_str());
      }
  }

  void
  throw_allocator_base::do_check_allocated(const_iterator beg,
					   const_iterator end,
					   size_t label)
  {
    std::string found;
    while (beg != end)
      {
	if (beg->second.first == label)
	  print_to_string(found, *beg);
	++beg;
      }

    if (!found.empty())
      {
	std::string error("throw_allocator_base::check_allocated by label \n");
	error += found;
	std::__throw_logic_error(error.c_str());
      }
  }

  void
  throw_allocator_base::print_to_string(std::string& s,
					const_reference ref)
  {
    char buf[40];
    const char tab('\t');
    s += "address: ";
    __builtin_sprintf(buf, "%p", ref.first);
    s += buf;
    s += tab;
    s += "label: ";
    unsigned long l = static_cast<unsigned long>(ref.second.first);
    __builtin_sprintf(buf, "%lu", l);
    s += buf;
    s += tab;
    s += "size: ";
    l = static_cast<unsigned long>(ref.second.second);
    __builtin_sprintf(buf, "%lu", l);
    s += buf;
    s += '\n';
  }

_GLIBCXX_END_NAMESPACE
