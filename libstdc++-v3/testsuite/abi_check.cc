// Utility for libstdc++ ABI analysis -*- C++ -*-

// Copyright (C) 2002 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Benjamin Kosnik  <bkoz@redhat.com>

#include <string>
#include <ext/hash_map>
#include <deque>
#include <sstream>
#include <fstream>
#include <iostream>
#include <cxxabi.h>

struct symbol_info
{
  enum category { none, function, object, error };
  category 	type;
  std::string 	name;
  std::string 	name_demangled;
  std::string 	version;
  int 		size;

  symbol_info() : type(none), size(0) { }

  symbol_info(const symbol_info& other) 
  : type(other.type), name(other.name), name_demangled(other.name_demangled), 
  version(other.version), size(other.size) { }
};

bool 
operator==(const symbol_info& lhs, const symbol_info& rhs)
{
  bool ret = true;

  // Check to see if symbol_infos are compatible.
  ret &= lhs.type == rhs.type;
  ret &= lhs.name == rhs.name;
  ret &= lhs.size == rhs.size;

  // Expect something more sophisticated eventually.
  ret &= lhs.version == rhs.version;
  return ret;
}

bool 
operator!=(const symbol_info& lhs, const symbol_info& rhs)
{ return !(lhs == rhs); }

template<typename _CharT, typename _Traits>
  std::basic_ostream<_CharT, _Traits>&
  operator<<(std::basic_ostream<_CharT, _Traits>& os, symbol_info& si)
  {
    using namespace std;
    os << si.type << endl;
    os << si.name << endl;
    os << si.name_demangled << endl;
    os << si.version << endl;
    os << si.size << endl;
    return os;
  }
 
const char*
demangle(const std::string& mangled)
{
  const char* name;
  if (mangled[0] != '_' && mangled[1] != 'Z')
    {
      // This is not a mangled symbol, thus has "C" linkage.
      name = mangled.c_str();
    }
  else
    {
      // Use __cxa_demangle to demangle.
      int status = 0;
      name = abi::__cxa_demangle(mangled.c_str(), 0, 0, &status);
      if (!name)
	{
	  switch (status)
	    {
	    case 0:
	      name = "error code = 0: success";
	      break;
	    case -1:
	      name = "error code = -1: memory allocation failure";
	      break;
	    case -2:
	      name = "error code = -2: invalid mangled name";
	      break;
	    case -3:
	      name = "error code = -3: invalid arguments";
	      break;
	    default:
	      name = "error code unknown - who knows what happened";
	    }
	}
    }
  return name;
}

void 
line_to_symbol_info(std::string& input, symbol_info& output)
{
  using namespace std;
  const char delim = ':';
  const char version_delim = '@';
  const string::size_type npos = string::npos;
  string::size_type n = 0;

  // Set the type.
  if (input.find("FUNC") == 0)
    output.type = symbol_info::function;
  else if (input.find("OBJECT") == 0)
    output.type = symbol_info::object;
  else
    output.type = symbol_info::error;
  n = input.find_first_of(delim);
  if (n != npos)
    input.erase(input.begin(), input.begin() + n + 1);

  // Iff object, get size info.
  if (output.type == symbol_info::object)
    {
      n = input.find_first_of(delim);
      if (n != npos)
	{
	  string size(input.begin(), input.begin() + n);
	  istringstream iss(size);
	  int x;
	  iss >> x;
	  if (iss.good())
	    output.size = x;
	  input.erase(input.begin(), input.begin() + n + 1);
	}
    }

  // Set the name.
  n = input.find_first_of(version_delim);
  if (n != npos)
    {
      // Found version string.
      output.name = string(input.begin(), input.begin() + n);
      n = input.find_last_of(version_delim);
      input.erase(input.begin(), input.begin() + n + 1);

      // Set version name.
      output.version = input;
    }
  else
    {
      // No versioning info.
      output.name = string(input.begin(), input.end());
      input.erase(input.begin(), input.end());
    }

  // Set the demangled name.
  output.name_demangled = demangle(output.name);
}

typedef std::deque<std::string>				symbol_names;
typedef __gnu_cxx::hash_map<const char*, symbol_info> 	symbol_infos;

void
collect_symbol_data(const char* file, symbol_infos& symbols, 
		    symbol_names& names)
{
  // Parse list of symbols in file into vectors of symbol_info.
  // For 3.2.0 on x86/linux, this usually is
  // 947 non-weak symbols
  // 2084 weak symbols
  using namespace std;
  ifstream ifs(file); 
  if (ifs.is_open())
    {
      // Organize input into container of symbol_info objects.
      const string empty;
      string line = empty;
      while (getline(ifs, line).good())
	{
	  symbol_info symbol;
	  line_to_symbol_info(line, symbol);
	  symbols[symbol.name.c_str()] = symbol;
	  names.push_back(symbol.name);
	  line = empty;
	}
    }
}


int main(int argc, char** argv)
{
  using namespace std;

  // Get arguments.
  if (argc != 2)
    {
      cerr << "Usage:  abi_check baseline_file" << endl;
      exit(1);
    }
  const char* baseline_file = argv[1];
  const char* test_file = "current_symbols.txt";
  const char* test_lib = "../src/.libs/libstdc++.so";

  // Get list of symbols.
  // Assume external symbol list computed "as if" by
  /*
   readelf -s -W libstdc++.so | sed '/\.dynsym/,/^$/p;d' | egrep -v
   ' (LOCAL|UND) ' | awk '{ if ($4 == "FUNC" || $4 == "NOTYPE") printf
   "%s:%s\n", $4, $8; else if ($4 == "OBJECT") printf "%s:%s:%s\n", $4,
   $3, $8;}' | sort >& current_symbols.txt
   */
  const char quote = '"';
  const char bslash = '\\';
  ostringstream cmd;
  cmd << "readelf -s -W " << test_lib << " | sed '/" << bslash 
      << ".dynsym/,/^$/p;d' | egrep -v ' (LOCAL|UND) ' | "
      << "awk '{ if ($4 == " << quote << "FUNC" << quote << "|| $4 == " 
      << quote << "NOTYPE" << quote << ") printf " << quote << "%s:%s"
      << bslash << "n" << quote << ", $4, $8; else if ($4 == " 
      << quote << "OBJECT" << quote << ") printf " << quote
      << "%s:%s:%s" << bslash << "n" << quote << ", $4, $3, $8;}' | "
      << "sort > " << test_file << " 2>&1";
  if (system(cmd.str().c_str()) != 0)
    {
      cerr << "Unable to generate the list of exported symbols." << endl;
      exit(2);
    }

  // Input both list of symbols into container.
  symbol_infos  baseline_symbols;
  symbol_names  baseline_names;
  symbol_infos  test_symbols;
  symbol_names  test_names;
  collect_symbol_data(baseline_file, baseline_symbols, baseline_names);
  collect_symbol_data(test_file, test_symbols, test_names);

  // Basic sanity check. (Was: error checking, what's that?)
  const symbol_names::size_type baseline_size = baseline_names.size();
  const symbol_names::size_type test_size = test_names.size();
  if (!baseline_size || !test_size)
    {
      cerr << "Problems parsing the list of exported symbols." << endl;
      exit(2);
    }

  // Sort out names.
  // Assuming baseline_names, test_names are both unique w/ no duplicates.
  //
  // The pairs of names in shared_names are needed to do lookups on
  // the hash tables of common symbols to do compares.
  //
  // The names added to missing_names are baseline_names not found in
  // test_names 
  // -> symbols that have been deleted.
  //
  // The names left in test_names are names not in baseline_names
  // -> symbols that have been added.
  typedef pair<string, string> string_pair;
  vector<string_pair> shared_names;
  symbol_names missing_names;
  for (size_t i = 0; i < baseline_size; ++i)
    {
      symbol_names::iterator end = test_names.end();
      symbol_names::iterator it = find(test_names.begin(), end, 
				       baseline_names[i]);
      if (it != end)
	{
	  // Found.
	  shared_names.push_back(string_pair(baseline_names[i], *it));
	  test_names.erase(it);
	}
      else
	missing_names.push_back(baseline_names[i]);
    }

  // Check common names for detailed compatibility.
  const vector<string_pair>::size_type shared_size = shared_names.size();
  typedef pair<symbol_info, symbol_info> symbol_pair;
  vector<symbol_pair> incompatible;
  for (size_t i = 0; i < shared_size; ++i)
    {
      symbol_info binfo = baseline_symbols[shared_names[i].first.c_str()];
      symbol_info tinfo = test_symbols[shared_names[i].second.c_str()];
      if (binfo != tinfo)
	incompatible.push_back(symbol_pair(binfo, tinfo));
    }

  // Output data.
  cout << test_names.size() << " added symbols " << endl;
  for (size_t j = 0; j < test_names.size() ; ++j)
    cout << '\t' << test_names[j] << endl;

  cout << missing_names.size() << " missing symbols " << endl;
  for (size_t j = 0; j < missing_names.size() ; ++j)
    cout << '\t' << missing_names[j] << endl;

  cout << incompatible.size() << " incompatible symbols " << endl;
  for (size_t j = 0; j < incompatible.size() ; ++j)
    {
      cout << "baseline symbol_info:" << endl;
      cout << incompatible[j].first << endl;
      cout << "test symbol_info:" << endl;
      cout << incompatible[j].second << endl;
    }

  return 0;
}
