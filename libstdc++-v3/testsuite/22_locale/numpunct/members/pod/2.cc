// 2003-07-09  Benjamin Kosnik  <bkoz@redhat.com>

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
#include <stdexcept>
#include <ext/pod_char_traits.h>
#include <testsuite_hooks.h>

typedef unsigned short					value_type;
typedef unsigned int					int_type;
typedef __gnu_cxx::character<value_type, int_type>	podchar_type;

// Member specializations for the existing facet classes.
// NB: This isn't especially portable. Perhaps a better way would be
// to just specialize all of numpunct and ctype.
namespace std
{
  template<>
    bool 
    ctype<podchar_type>::
    do_is(mask, char_type) const { return true; }

  template<>
    const podchar_type*
    ctype<podchar_type>::
    do_is(const char_type* __lo, const char_type*, mask*) const
    { return __lo; }

  template<>
    const podchar_type*
    ctype<podchar_type>::
    do_scan_is(mask, const char_type* __lo, const char_type*) const
    { return __lo; }

  template<>
    const podchar_type*
    ctype<podchar_type>::
    do_scan_not(mask, const char_type* __lo, const char_type*) const
    { return __lo; }

  template<>
    podchar_type 
    ctype<podchar_type>::
    do_toupper(char_type __c) const
    { return __c; }

  template<>
    const podchar_type*
    ctype<podchar_type>::
    do_toupper(char_type*, const char_type* __hi) const
    { return __hi; }

  template<>
    podchar_type 
    ctype<podchar_type>::
    do_tolower(char_type __c) const
    { return __c; }

  template<>
    const podchar_type*
    ctype<podchar_type>::
    do_tolower(char_type*, const char_type* __hi) const
    { return __hi; }

  template<>
    podchar_type
    ctype<podchar_type>::
    do_widen(char __c) const
    { 
      char_type ret = { value_type(__c) };
      return ret;
    }

  template<>
    const char*
    ctype<podchar_type>::
    do_widen(const char* __lo, const char* __hi, char_type* __dest) const
    {
      while (__lo < __hi)
	{
	  *__dest = this->do_widen(*__lo);
	  ++__lo;
	  ++__dest;
	}
      return __hi;
    }

  template<>
    char
    ctype<podchar_type>::
    do_narrow(char_type __wc, char) const
    { return static_cast<char>(__wc.value); }

  template<>
    const podchar_type*
    ctype<podchar_type>::
    do_narrow(const podchar_type* __lo, const podchar_type* __hi, 
	      char, char* __dest) const
    {
      while (__lo < __hi)
	{
	  *__dest = this->do_narrow(*__lo, char());
	  ++__lo;
	  ++__dest;
	}
      return __hi;
    }

  template<>
    ctype<podchar_type>::~ctype() { }

  template<>
    void
    numpunct<podchar_type>::_M_initialize_numpunct(__c_locale)
    {
      if (!_M_data)
	_M_data = new __numpunct_cache<podchar_type>;

      _M_data->_M_grouping = "";
      _M_data->_M_use_grouping = false;

      _M_data->_M_decimal_point.value =  value_type('.');
      _M_data->_M_thousands_sep.value = value_type(',');
      
      for (size_t i = 0; i < __num_base::_S_oend; ++i)
	{
	  value_type v = __num_base::_S_atoms_out[i];
	  _M_data->_M_atoms_out[i].value = v;
	}
      _M_data->_M_atoms_out[__num_base::_S_oend] = podchar_type();
      
      for (size_t i = 0; i < __num_base::_S_iend; ++i)
	_M_data->_M_atoms_in[i].value = value_type(__num_base::_S_atoms_in[i]);
      _M_data->_M_atoms_in[__num_base::_S_iend] = podchar_type();

      // "true"
      podchar_type* __truename = new podchar_type[4 + 1];
      __truename[0].value = value_type('t');
      __truename[1].value = value_type('r');
      __truename[2].value = value_type('u');
      __truename[3].value = value_type('e');
      __truename[4] = podchar_type();
      _M_data->_M_truename = __truename;

      // "false"
      podchar_type* __falsename = new podchar_type[5 + 1];
      __falsename[0].value = value_type('f');
      __falsename[1].value = value_type('a');
      __falsename[2].value = value_type('l');
      __falsename[3].value = value_type('s');
      __falsename[4].value = value_type('e');
      __falsename[5] = podchar_type();
      _M_data->_M_falsename = __falsename;
    }

  template<>
    numpunct<podchar_type>::~numpunct()
    { delete _M_data; }
}

// Check for numpunct and ctype dependencies. Make sure that numpunct
// can be created without ctype.
void test01()
{
  using namespace std;
  typedef numpunct<podchar_type>::string_type 	string_type;
  typedef basic_ostringstream<podchar_type> 		ostream_type;
  
  bool 		test = true;

  // Test formatted output.
  ostream_type 		os;
  const locale 	loc = locale::classic();
  os.imbue(loc);
  os.setf(ios_base::boolalpha);
  os.exceptions(ios_base::badbit);

  // 1: fail, no num_put.
  try
    {
      // Calls to num_put.put will fail, as there's no num_put facet.
      os << true;
      test = false;
    }
  catch(const bad_cast& obj)
    { }
  catch(...)
    { test = false; }
  VERIFY( test );

  // 2: fail, no ctype
  const locale 	loc2(loc, new num_put<podchar_type>);
  os.clear();
  os.imbue(loc2);
  try
    {
      // Calls to ctype.widen will fail, as there's no ctype facet.
      os << true;
      test = false;
    }
  catch(const bad_cast& obj)
    { }
  catch(...)
    { test = false; }
  VERIFY( test );

  // 3: fail, no numpunct
  const locale 	loc3(loc, new ctype<podchar_type>);
  os.clear();
  os.imbue(loc3);
  try
    {
      // Formatted output fails as no numpunct.
      os << true;
      test = false;
    }
  catch(const bad_cast& obj)
    { }
  catch(...)
    { test = false; }
  VERIFY( test );

  // 4: works.
  const locale 	loc4(loc3, new numpunct<podchar_type>);
  os.clear();
  os.imbue(loc4);
  try
    {
      os << long(500);
      string_type s = os.str();
      VERIFY( s.length() == 3 );

      VERIFY( os.narrow(s[0], char()) == '5' );
      VERIFY( os.narrow(s[1], char()) == '0' );
      VERIFY( os.narrow(s[2], char()) == '0' );
    }
  catch(const bad_cast& obj)
    { test = false; }
  catch(...)
    { test = false; }
  VERIFY( test );
}

int main()
{
  test01();
  return 0;
}
