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
  typedef basic_stringbuf<podchar_type> 	stringbuf_type;
  typedef basic_ostream<podchar_type> 		ostream_type;
  
  bool test __attribute__((unused)) = true;

  // Pre-cache sanity check.
  const locale 	loc(locale::classic(), new numpunct<podchar_type>);
  const numpunct<podchar_type>& np = use_facet<numpunct<podchar_type> >(loc);

  podchar_type dp = np.decimal_point();
  podchar_type ts = np.thousands_sep();
  string g = np.grouping();
  string_type strue = np.truename();
  string_type sfalse = np.falsename();

  podchar_type basedp = { value_type('.') };
  podchar_type basets = { value_type(',') };

  string_type basetrue(4, podchar_type());
  basetrue[0].value = value_type('t');
  basetrue[1].value = value_type('r');
  basetrue[2].value = value_type('u');
  basetrue[3].value = value_type('e');

  string_type basefalse(5, podchar_type());
  basefalse[0].value = value_type('f');
  basefalse[1].value = value_type('a');
  basefalse[2].value = value_type('l');
  basefalse[3].value = value_type('s');
  basefalse[4].value = value_type('e');

  VERIFY( char_traits<podchar_type>::eq(dp, basedp) );
  VERIFY( char_traits<podchar_type>::eq(ts, basets) );
  VERIFY( g == "" );
  VERIFY( strue == basetrue );
  VERIFY( sfalse == basefalse );
}

int main()
{
  test01();
  return 0;
}
