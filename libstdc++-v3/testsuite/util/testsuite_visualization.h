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

#include <typeinfo>
#include <stdexcept>
#include <sstream>
#include <fstream>
#include <cxxabi.h>
#include <testsuite_performance.h>

// Ah, we wish it wasn't so...
bool first_container = false;
extern const char* filename;

typedef std::string::size_type (*callback_type) (std::string&);

template<typename Container, int Iter, bool Thread>
  void
  write_viz_container(callback_type find_container, const char* filename)
  {
    typedef std::string string;

    // Create title.
    {
      const char ws(' ');
      std::ostringstream title;
	
      std::string titlename(filename);
      std::string::size_type n = titlename.find('.');
      if (n != string::npos)
	titlename = std::string(titlename.begin(), titlename.begin() + n);

      title << titlename;
      title << ws;
      title << Iter;
      title << ws;
#if 0
      title << "thread<";
      std::boolalpha(title);
      title << Thread;
      title << '>';
#endif
      
      titlename += ".title";
      std::ofstream titlefile(titlename.c_str());
      if (!titlefile.good())
	throw std::runtime_error("write_viz_data cannot open titlename");
      titlefile << title.str() << std::endl;
    }

    // Create compressed type name.
    Container obj;
    int status;
    std::string type(abi::__cxa_demangle(typeid(obj).name(), 0, 0, &status));
    
    // Extract fully-qualified typename.
    // Assumes "set" or "map" are uniquely determinate.
    string::iterator beg = type.begin();
    string::iterator end;
    string::size_type n = (*find_container)(type);

    // Find start of fully-qualified name.
    // Assume map, find end.
    string::size_type nend = type.find('<', n);
    if (nend != string::npos)
      end = type.begin() + nend;
    
    string compressed_type;
    compressed_type += '"';
    compressed_type += string(beg, end);
    compressed_type += '<';
#if 0
    typename Container::key_type v;
    compressed_type += typeid(v).name();
#else
    compressed_type += "int";
#endif
    compressed_type += ", A>";

    // XXX
    if (Thread == true)
      compressed_type += " thread";
    compressed_type += '"';

    std::ofstream file(filename, std::ios_base::app);
    if (!file.good())
      throw std::runtime_error("write_viz_data cannot open filename");
    
    file << compressed_type;
    first_container = false;
  }


void
write_viz_data(__gnu_test::time_counter& time, const char* filename)
{
  std::ofstream file(filename, std::ios_base::app);  
  if (!file.good())
    throw std::runtime_error("write_viz_data cannot open filename");
  
  // Print out score in appropriate column.
  const char tab('\t');
  int score = time.real_time();
  file << tab << score;
}

void
write_viz_endl(const char* filename)
{
  std::ofstream file(filename, std::ios_base::app);
  if (!file.good())
    throw std::runtime_error("write_viz_endl cannot open filename");
  file << std::endl;
}


#if 0
// cons
write_viz_container<container_type, Iter, Thread>(&sequence_find_container, 
						  filename);
#endif

#if 0
// dtor
write_viz_endl(filename)
#endif
