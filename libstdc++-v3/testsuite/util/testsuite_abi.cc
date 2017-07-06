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

#include "testsuite_abi.h"
#include <cstdlib>
#include <sstream>
#include <fstream>
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

void
symbol::init(string& data)
{
  const char delim = ':';
  const char version_delim = '@';
  const string::size_type npos = string::npos;
  string::size_type n = 0;

  // Set the type.
  if (data.find("FUNC") == 0)
    type = symbol::function;
  else if (data.find("OBJECT") == 0)
    type = symbol::object;
  else if (data.find("TLS") == 0)
    type = symbol::tls;

  n = data.find_first_of(delim);
  if (n != npos)
    data.erase(data.begin(), data.begin() + n + 1);

  // Iff object or TLS, get size info.
  if (type == symbol::object || type == symbol::tls)
    {
      n = data.find_first_of(delim);
      if (n != npos)
	{
	  string objectsize(data.begin(), data.begin() + n);
	  istringstream iss(objectsize);
	  int x;
	  iss >> x;
	  if (!iss.fail())
	    size = x;
	  data.erase(data.begin(), data.begin() + n + 1);
	}
    }

  // Set the name and raw_name.
  raw_name = string(data.begin(), data.end());
  n = data.find_first_of(version_delim);
  if (n != npos)
    {
      // Found version string.
      name = string(data.begin(), data.begin() + n);
      n = data.find_last_of(version_delim);
      data.erase(data.begin(), data.begin() + n + 1);

      // Set version name.
      version_name = data;
    }
  else
    {
      // No versioning info.
      name = string(data.begin(), data.end());
      version_status = symbol::none;
    }

  // Set the demangled name.
  demangled_name = demangle(name);
}

void
symbol::print() const
{
  const char tab = '\t';
  cout << name << endl;

  if (demangled_name != name)
    cout << demangled_name << endl;

  string vers;
  switch (version_status)
    {
    case none:
      vers = "none";
      break;
    case compatible:
      vers = "compatible";
      break;
    case incompatible:
      vers = "incompatible";
      break;
     case unversioned:
      vers = "unversioned";
      break;
   default:
      vers = "<default>";
    }
  cout << "version status: " << vers << endl;

  if (version_name.size()
      && (version_status == compatible || version_status == incompatible))
    cout << version_name << endl;

  string type_string;
  switch (type)
    {
    case function:
      type_string = "function";
      break;
    case object:
      type_string = "object";
      break;
    case tls:
      type_string = "tls";
      break;
    case uncategorized:
      type_string = "uncategorized";
      break;
    default:
      type_string = "<default>";
    }
  cout << "type: " << type_string << endl;

  if (type == object || type == tls)
    cout << "type size: " << size << endl;

  string status_string;
  switch (status)
    {
    case added:
      status_string = "added";
      break;
    case subtracted:
      status_string = "subtracted";
      break;
    case undesignated:
      status_string = "undesignated";
      break;
    default:
      status_string = "<default>";
    }
  cout << "status: " << status_string << endl;

  cout << endl;
}


bool
check_version(symbol& test, bool added)
{
  // Construct list of compatible versions.
  typedef std::vector<std::string> compat_list;
  static compat_list known_versions;
  if (known_versions.empty())
    {
      // NB: First version here must be the default version for this
      // version of DT_SONAME.
      known_versions.push_back("GLIBCXX_3.4");
      known_versions.push_back("GLIBCXX_LDBL_3.4");
      known_versions.push_back("GLIBCXX_3.4.1");
      known_versions.push_back("GLIBCXX_3.4.2");
      known_versions.push_back("GLIBCXX_3.4.3");
      known_versions.push_back("GLIBCXX_3.4.4");
      known_versions.push_back("GLIBCXX_3.4.5");
      known_versions.push_back("GLIBCXX_3.4.6");
      known_versions.push_back("GLIBCXX_3.4.7");
      known_versions.push_back("GLIBCXX_LDBL_3.4.7");
      known_versions.push_back("GLIBCXX_3.4.8");
      known_versions.push_back("GLIBCXX_3.4.9");
      known_versions.push_back("GLIBCXX_3.4.10");
      known_versions.push_back("GLIBCXX_LDBL_3.4.10");
      known_versions.push_back("GLIBCXX_3.4.11");
      known_versions.push_back("GLIBCXX_3.4.12");
      known_versions.push_back("GLIBCXX_3.4.13");
      known_versions.push_back("GLIBCXX_3.4.14");
      known_versions.push_back("GLIBCXX_3.4.15");
      known_versions.push_back("GLIBCXX_3.4.16");
      known_versions.push_back("GLIBCXX_3.4.17");
      known_versions.push_back("GLIBCXX_3.4.18");
      known_versions.push_back("GLIBCXX_3.4.19");
      known_versions.push_back("GLIBCXX_3.4.20");
      known_versions.push_back("GLIBCXX_3.4.21");
      known_versions.push_back("GLIBCXX_LDBL_3.4.21");
      known_versions.push_back("GLIBCXX_3.4.22");
      known_versions.push_back("GLIBCXX_3.4.23");
      known_versions.push_back("GLIBCXX_3.4.24");
      known_versions.push_back("GLIBCXX_3.4.25");
      known_versions.push_back("CXXABI_1.3");
      known_versions.push_back("CXXABI_LDBL_1.3");
      known_versions.push_back("CXXABI_1.3.1");
      known_versions.push_back("CXXABI_1.3.2");
      known_versions.push_back("CXXABI_1.3.3");
      known_versions.push_back("CXXABI_1.3.4");
      known_versions.push_back("CXXABI_1.3.5");
      known_versions.push_back("CXXABI_1.3.6");
      known_versions.push_back("CXXABI_1.3.7");
      known_versions.push_back("CXXABI_1.3.8");
      known_versions.push_back("CXXABI_1.3.9");
      known_versions.push_back("CXXABI_1.3.10");
      known_versions.push_back("CXXABI_1.3.11");
      known_versions.push_back("CXXABI_TM_1");
      known_versions.push_back("CXXABI_FLOAT128");
    }
  compat_list::iterator begin = known_versions.begin();
  compat_list::iterator end = known_versions.end();

  // Check for compatible version.
  if (test.version_name.size())
    {
      compat_list::iterator it1 = find(begin, end, test.version_name);
      compat_list::iterator it2 = find(begin, end, test.name);
      if (it1 != end)
	test.version_status = symbol::compatible;
      else
	test.version_status = symbol::incompatible;

      // Check that added symbols are added in the latest pre-release version.
      bool latestp = (test.version_name == "GLIBCXX_3.4.25"
		     || test.version_name == "CXXABI_1.3.11"
		     || test.version_name == "CXXABI_FLOAT128"
		     || test.version_name == "CXXABI_TM_1");
      if (added && !latestp)
	test.version_status = symbol::incompatible;

      // Check that long double compatibility symbols demangled as
      // __float128 and regular __float128 symbols are put into some _LDBL_
      // or _FLOAT128 version name.
      if (added && test.demangled_name.find("__float128") != std::string::npos
	  && test.demangled_name.find("std::__cxx11::") != 0)
	{
	  if (test.version_name.find("_LDBL_") == std::string::npos
	      && test.version_name.find("_FLOAT128") == std::string::npos)
	    test.version_status = symbol::incompatible;
	}

      // Check for weak label.
      if (it1 == end && it2 == end)
	test.version_status = symbol::incompatible;

      // Check that
      // GLIBCXX_3.4
      // GLIBCXX_3.4.5
      // version as compatible
      // XXX
    }
  else
    {
      if (added)
	{
	  // New version labels are ok. The rest are not.
	  compat_list::iterator it2 = find(begin, end, test.name);
	  if (it2 != end)
	    test.version_status = symbol::compatible;
	  else
	    test.version_status = symbol::incompatible;
	}
    }
  return test.version_status == symbol::compatible;
}

bool
check_compatible(symbol& lhs, symbol& rhs, bool verbose)
{
  bool ret = true;
  const char tab = '\t';

  // Check to see if symbol_objects are compatible.
  if (lhs.type != rhs.type)
    {
      ret = false;
      if (verbose)
	cout << tab << "incompatible types" << endl;
    }

  if (lhs.name != rhs.name)
    {
      ret = false;
      if (verbose)
	cout << tab << "incompatible names" << endl;
    }

  if (lhs.size != rhs.size)
    {
      ret = false;
      if (verbose)
	{
	  cout << tab << "incompatible sizes" << endl;
	  cout << tab << lhs.size << endl;
	  cout << tab << rhs.size << endl;
	}
    }

  if (lhs.version_name != rhs.version_name
      && !check_version(lhs) && !check_version(rhs))
    {
      ret = false;
      if (verbose)
	{
	  cout << tab << "incompatible versions" << endl;
	  cout << tab << lhs.version_name << endl;
	  cout << tab << rhs.version_name << endl;
	}
    }

  if (verbose)
    cout << endl;

  return ret;
}


inline bool
has_symbol(const string& name, const symbols& s) throw()
{ return s.find(name) != s.end(); }

const symbol&
get_symbol(const string& name, const symbols& s)
{
  symbols::const_iterator i = s.find(name);
  if (i != s.end())
    {
      return i->second;
    }
  else
    {
      ostringstream os;
      os << "get_symbol failed for symbol " << name;
      __throw_logic_error(os.str().c_str());
    }
}

void
examine_symbol(const char* name, const char* file)
{
  try
    {
      symbols s = create_symbols(file);
      const symbol& sym = get_symbol(name, s);
      sym.print();
    }
  catch(...)
    { __throw_exception_again; }
}

int
compare_symbols(const char* baseline_file, const char* test_file,
		bool verbose)
{
  // Input both lists of symbols into container.
  symbols baseline = create_symbols(baseline_file);
  symbols test = create_symbols(test_file);

  //  Sanity check results.
  if (!baseline.size() || !test.size())
    {
      cerr << "Problems parsing the list of exported symbols." << endl;
      exit(2);
    }

  // Check to see if any long double compatibility symbols are produced.
  bool ld_version_found(false);
  symbols::iterator li(test.begin());
  while (!ld_version_found && li != test.end())
    {
      if (li->second.version_name.find("_LDBL_") != std::string::npos)
	ld_version_found = true;
      ++li;
    }

  // Sort out names.
  // Assuming all baseline names and test names are both unique w/ no
  // duplicates.
  //
  // The names added to missing_names are baseline names not found in
  // test names
  // -> symbols that have been deleted.
  //
  // The names added to added_names are test names not in
  // baseline names
  // -> symbols that have been added.
  typedef std::vector<std::string> symbol_names;
  symbol_names shared_names;
  symbol_names missing_names;
  symbol_names added_names;
  for (li = test.begin(); li != test.end(); ++li)
    added_names.push_back(li->first);

  for (symbols::iterator i = baseline.begin(); i != baseline.end(); ++i)
    {
      string name(i->first);
      symbol_names::iterator end = added_names.end();
      symbol_names::iterator it = find(added_names.begin(), end, name);
      if (it != end)
	{
	  // Found.
	  shared_names.push_back(name);
	  added_names.erase(it);
	}
       else
	{
	  // Iff no test long double compatibility symbols at all and the symbol
	  // missing is a baseline long double compatibility symbol, skip.
	  string version_name(i->second.version_name);
	  bool base_ld(version_name.find("_LDBL_") != std::string::npos);
	  if (!base_ld || base_ld && ld_version_found)
	    missing_names.push_back(name);
	}
    }

  // Fill out list of incompatible symbols.
  typedef pair<symbol, symbol> symbol_pair;
  vector<symbol_pair> incompatible;

  // Fill out list of undesignated symbols.
  vector<symbol> undesignated;

  // Check missing names for compatibility.
  for (size_t j = 0; j < missing_names.size(); ++j)
    {
      symbol& sbase = baseline[missing_names[j]];
      sbase.status = symbol::subtracted;
      incompatible.push_back(symbol_pair(sbase, sbase));
    }

  // Check shared names for compatibility.
  const symbol_names::size_type shared_size = shared_names.size();
  for (size_t k = 0; k < shared_size; ++k)
    {
      symbol& sbase = baseline[shared_names[k]];
      symbol& stest = test[shared_names[k]];
      stest.status = symbol::existing;
      if (!check_compatible(sbase, stest))
	incompatible.push_back(symbol_pair(sbase, stest));
    }

  // Check added names for compatibility.
  const symbol_names::size_type added_size = added_names.size();
  for (size_t l = 0; l < added_size; ++l)
    {
      symbol& stest = test[added_names[l]];

      // Mark TLS as undesignated, remove from added.
      if (stest.type == symbol::tls)
	{
	  stest.status = symbol::undesignated;
	  if (!check_version(stest, false))
	    incompatible.push_back(symbol_pair(stest, stest));
	  else
	    undesignated.push_back(stest);
	}
      else
	{
	  stest.status = symbol::added;
	  if (!check_version(stest, true))
	    incompatible.push_back(symbol_pair(stest, stest));
	}
    }

  // Normalize added names and undesignated names.
  const size_t undesignated_size = undesignated.size();
  for (size_t l = 0; l < undesignated_size; ++l)
    {
      symbol& sundes = undesignated[l];
      symbol_names::iterator end = added_names.end();
      symbol_names::iterator it = find(added_names.begin(), end, sundes.name);
       if (it != end)
	{
	  // Found.
	  added_names.erase(it);
	}
       else
	 __throw_runtime_error(sundes.name.c_str());
    }


  // Report results.
  if (verbose && added_names.size())
    {
      cout << endl << added_names.size() << " added symbols " << endl;
      for (size_t j = 0; j < added_names.size() ; ++j)
	{
	  cout << j << endl;
	  test[added_names[j]].print();
	}
    }

  if (verbose && missing_names.size())
    {
      cout << endl << missing_names.size() << " missing symbols " << endl;
      for (size_t j = 0; j < missing_names.size() ; ++j)
	{
	  cout << j << endl;
	  baseline[missing_names[j]].print();
	}
    }

  if (verbose && undesignated.size())
    {
      cout << endl << undesignated.size() << " undesignated symbols " << endl;
      for (size_t j = 0; j < undesignated.size() ; ++j)
	{
	  // First, print index.
	  cout << j << endl;

	  // Second, report name.
	  symbol& s = undesignated[j];
	  s.print();
	}
    }

  if (verbose && incompatible.size())
    {
      cout << endl << incompatible.size() << " incompatible symbols " << endl;
      for (size_t j = 0; j < incompatible.size() ; ++j)
	{
	  // First, print index.
	  cout << j << endl;

	  // Second, report name.
	  symbol& sbase = incompatible[j].first;
	  symbol& stest = incompatible[j].second;
	  stest.print();

	  // Third, report reason or reasons incompatible.
	  check_compatible(sbase, stest, true);
	}
    }

  cout << "\n\t\t==== libstdc++-v3 check-abi Summary ====" << endl;
  cout << endl;
  cout << "# of added symbols:\t\t " << added_names.size() << endl;
  cout << "# of missing symbols:\t\t " << missing_names.size() << endl;
  cout << "# of undesignated symbols:\t " << undesignated.size() << endl;
  cout << "# of incompatible symbols:\t " << incompatible.size() << endl;
  cout << endl;
  cout << "using: " << baseline_file << endl;

  return !(missing_names.size() || incompatible.size());
}


symbols
create_symbols(const char* file)
{
  symbols s;
  ifstream ifs(file);
  if (ifs.is_open())
    {
      // Organize file data into an associated container (symbols) of symbol
      // objects mapped to mangled names without versioning
      // information.
      const string empty;
      string line = empty;
      while (getline(ifs, line).good())
	{
	  symbol tmp;
	  tmp.init(line);
	  s[tmp.name] = tmp;
	  line = empty;
	}
    }
  else
    {
      ostringstream os;
      os << "create_symbols failed for file " << file;
      __throw_runtime_error(os.str().c_str());
    }
  return s;
}


std::string
demangle(const std::string& mangled)
{
  std::string name;
  if (mangled[0] != '_' || mangled[1] != 'Z')
    {
      // This is not a mangled symbol, thus has "C" linkage.
      name = mangled;
    }
  else
    {
      // Use __cxa_demangle to demangle.
      int status = 0;
      char* ptr = abi::__cxa_demangle(mangled.c_str(), 0, 0, &status);
      if (ptr)
	{
	  name = ptr;
	  free(ptr);
	}
      else
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
