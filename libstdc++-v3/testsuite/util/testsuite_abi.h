// -*- C++ -*-

// Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

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
// the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
// MA 02110-1301, USA.

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
#include <locale>
#include <tr1/unordered_map>
#include <cxxabi.h>

// Encapsulates symbol characteristics.
struct symbol
{
  enum category { function, object, uncategorized };
  enum designation { existing, added, subtracted, undesignated };
  enum version { none, compatible, incompatible, unversioned };
  enum compatibility 
    { 
      compat_type = 1, 
      compat_name = 2, 
      compat_size = 4, 
      compat_version = 8 
    };

  category 	type;
  std::string 	name;
  std::string 	raw_name; // Name with versioning info still attached.
  std::string 	demangled_name;
  int 		size;
  std::string 	version_name;
  version	version_status;
  designation	status;

  symbol() 
  : type(uncategorized), size(0), version_status(unversioned), 
    status(undesignated) { }

  symbol(const symbol& other) 
  : type(other.type), name(other.name), demangled_name(other.demangled_name), 
    size(other.size), version_name(other.version_name), 
    version_status(other.version_status), status(other.status) { }

  void
  print() const;

  void
  init(std::string& data);
};

typedef std::tr1::unordered_map<std::string, symbol> 	symbol_objects;

typedef std::deque<std::string>				symbol_names;

typedef std::pair<symbol_names, symbol_objects>		symbols;


// Check.
bool
check_version(symbol& test, bool added = false);

bool 
check_compatible(symbol& lhs, symbol& rhs, bool verbose = false);


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
