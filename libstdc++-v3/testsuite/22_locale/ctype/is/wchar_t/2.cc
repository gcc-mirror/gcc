// { dg-do run { xfail *-*-![linux]* } }
// Copyright (C) 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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

// 22.2.1.3.2 ctype<char> members

#include <locale>
#include <vector>
#include <testsuite_hooks.h>

typedef wchar_t char_type;

// libstdc++/4456, libstdc++/4457, libstdc++/4458
void test02()
{
  using namespace std;
  typedef ctype_base::mask 	mask;
  typedef vector<mask> 		vector_type;

  bool test __attribute__((unused)) = true;

  //  const int max = numeric_limits<char>::max();
  const int max = 255;
  const int ctype_mask_max = 10;
  vector_type v_c(max);
  vector_type v_de(max);

  // "C"
  locale loc_c = locale::classic();
  const ctype<wchar_t>& ctype_c = use_facet<ctype<wchar_t> >(loc_c); 
  for (int i = 0; i < max; ++i)
    {
      char_type c = static_cast<wchar_t>(i);
      mask mask_test = static_cast<mask>(0);
      mask mask_is = static_cast<mask>(0);
      for (int j = 0; j <= ctype_mask_max; ++j)
	{
	  mask_test = static_cast<mask>(1 << j);
	  if (ctype_c.is(mask_test, c))
	    mask_is |= mask_test;
	}
      v_c[i] = mask_is;
    }   

  // "de_DE"
  locale loc_de = __gnu_test::try_named_locale("de_DE");
  const ctype<wchar_t>& ctype_de = use_facet<ctype<wchar_t> >(loc_de); 
  for (int i = 0; i < max; ++i)
    {
      char_type c = static_cast<wchar_t>(i);
      mask mask_test = static_cast<mask>(0);
      mask mask_is = static_cast<mask>(0);
      for (int j = 0; j <= ctype_mask_max; ++j)
	{
	  mask_test = static_cast<mask>(1 << j);
	  if (ctype_de.is(mask_test, c))
	    mask_is |= mask_test;
	}
      v_de[i] = mask_is;
    }   

#if QUANNUM_VERBOSE_LYRICALLY_ADEPT_BAY_AREA_MCS_MODE
    for (int i = 0; i < max; ++i)
    {
      char_type mark = v_c[i] == v_de[i] ? ' ' : '-';
      cout << i << ' ' << mark << ' ' << static_cast<wchar_t>(i) << '\t' ;
      cout << "v_c: " << setw(4) << v_c[i] << '\t';
      cout << "v_de: " << setw(4) << v_de[i] << endl;
    }
    cout << (v_c == v_de) << endl;
#endif

  VERIFY( v_c != v_de );
}

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<std::ctype_base::mask>;
#endif

int main() 
{
  test02();
  return 0;
}
