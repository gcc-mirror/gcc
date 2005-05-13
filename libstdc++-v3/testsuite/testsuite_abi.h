// -*- C++ -*-

// Copyright (C) 2004 Free Software Foundation, Inc.

// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2, or (at
// your option) any later version.

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

// Benjamin Kosnik  <bkoz@redhat.com>

#include <string>
#include <stdexcept>
#include <deque>
#include <ext/hash_map>
#include <cxxabi.h>

// Encapsulates symbol characteristics.
struct symbol
{
  enum category { none, function, object, error };
  enum designation { unknown, added, subtracted, compatible, incompatible };
  enum compatibility 
    { 
      compat_type = 1, 
      compat_name = 2, 
      compat_size = 4, 
      compat_version = 8 
    };

  category 	type;
  std::string 	name;
  std::string 	demangled_name;
  int 		size;
  std::string 	version_name;
  designation	status;

  symbol() : type(none), size(0), status(unknown) { }

  symbol(const symbol& other) 
  : type(other.type), name(other.name), demangled_name(other.demangled_name), 
   size(other.size), version_name(other.version_name),
   status(other.status) { }

  void
  print() const;

  void
  init(std::string& data);
};

struct symbol_error : public std::logic_error
{
  explicit symbol_error(const std::string& s) : std::logic_error(s) { }
};


typedef __gnu_cxx::hash_map<std::string, symbol> 	symbol_objects;

typedef std::deque<std::string>				symbol_names;

typedef std::pair<symbol_names, symbol_objects>		symbols;


// Check.
bool
check_version(const symbol& test, bool added = false);

bool 
check_compatible(const symbol& lhs, const symbol& rhs, bool verbose = false);


// Examine.
bool
has_symbol(const std::string& mangled, const symbols& list) throw();

symbol&
get_symbol(const std::string& mangled, const symbols& list);

extern "C" void
examine_symbol(const char* name, const char* file);

extern "C" int
compare_symbols(const char* baseline_file, const char* test_file, bool verb);


// Util.
symbols
create_symbols(const char* file);

const char*
demangle(const std::string& mangled);


// Specialization.
namespace __gnu_cxx
{
  using namespace std;

  template<> 
    struct hash<string>
    {
      size_t operator()(const string& s) const 
      { 
	const collate<char>& c = use_facet<collate<char> >(locale::classic());
	return c.hash(s.c_str(), s.c_str() + s.size());
      }
    }; 
}
