// 2003-07-06  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2003 Free Software Foundation, Inc.
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

#include <locale>
#include <sstream>
#include <ostream>
#include <testsuite_hooks.h>

class numpunct_checked : public std::numpunct<wchar_t>
{
  typedef std::numpunct<wchar_t> base;

public:
  explicit 
  numpunct_checked(std::size_t refs = 0): base(refs) { }

  string_type
  base_truename() const
  { return base::do_truename(); }

protected:
  virtual string_type  
  do_truename() const
  { return base::do_truename() + L"st"; }
};

// Thwart locale caching strategies that incorrectly overwrite base
// class data.
void test01()
{
  using namespace std;

  bool test __attribute__((unused)) = true;
  const wstring 	basestr(L"true");
  const wstring 	derivedstr(L"truest");

  const locale 	loc(locale::classic(), new numpunct_checked);	
  wstringbuf 	sbuf;
  wostream 	os(&sbuf);
  os.setf(ios_base::boolalpha);

  // Pre-cache sanity check.
  const numpunct<wchar_t>& np = use_facet<numpunct<wchar_t> >(loc);
  VERIFY( np.truename() == derivedstr );
  
  // Cache.
  os.imbue(loc);
  os << true;
  VERIFY( sbuf.str() == derivedstr );

  // Post-cache sanity check, make sure that base class is still fine.
  VERIFY( np.truename() == derivedstr );
  const numpunct_checked& npd = static_cast<const numpunct_checked&>(np);
  VERIFY( npd.base_truename() == basestr );
}

int main()
{
  test01();
  return 0;
}
