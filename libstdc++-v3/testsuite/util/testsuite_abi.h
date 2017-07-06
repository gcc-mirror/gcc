// -*- C++ -*-

// Copyright (C) 2004-2017 Free Software Foundation, Inc.

// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 3, or (at
// your option) any later version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// Benjamin Kosnik  <bkoz@redhat.com>

#include <string>
#include <stdexcept>
#include <vector>
#include <locale>
#include <tr1/unordered_map>
#include <cxxabi.h>

// Encapsulates symbol characteristics.
struct symbol
{
  enum category { function, object, tls, uncategorized };
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

// Map type between symbol names and full symbol info.
typedef std::tr1::unordered_map<std::string, symbol> 	symbols;


// Check.
bool
check_version(symbol& test, bool added = false);

bool
check_compatible(symbol& lhs, symbol& rhs, bool verbose = false);


// Examine.
bool
has_symbol(const std::string& mangled, const symbols& list) throw();

const symbol&
get_symbol(const std::string& mangled, const symbols& list);

extern "C" void
examine_symbol(const char* name, const char* file);

extern "C" int
compare_symbols(const char* baseline_file, const char* test_file, bool verb);


// Util.
symbols
create_symbols(const char* file);

std::string
demangle(const std::string& mangled);
