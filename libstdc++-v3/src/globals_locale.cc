// Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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

#include <locale>

// On AIX, and perhaps other systems, library initialization order is
// not guaranteed.  For example, the static initializers for the main
// program might run before the static initializers for this library.
// That means that we cannot rely on static initialization in the
// library; there is no guarantee that things will get initialized in
// time.  This file contains definitions of all global variables that
// require initialization as arrays of characters.

// NB: asm directives can rename these non-exported, namespace
// __gnu_cxx symbols into exported, namespace std symbols with the
// appropriate symbol version name.
// The rename syntax is 
//   asm (".symver currentname,oldname@@GLIBCXX_3.2")
// In macro form:
// _GLIBCXX_ASM_SYMVER(currentname, oldname, GLIBCXX_3.2)

namespace __gnu_internal
{
  using namespace std;

  typedef char fake_locale_Impl[sizeof(locale::_Impl)]
  __attribute__ ((aligned(__alignof__(locale::_Impl))));
  fake_locale_Impl c_locale_impl;

  typedef char fake_locale[sizeof(locale)]
  __attribute__ ((aligned(__alignof__(locale))));
  fake_locale c_locale;

  typedef char fake_name_vec[sizeof(char*)]
  __attribute__ ((aligned(__alignof__(char*))));
  fake_name_vec name_vec[6 + _GLIBCXX_NUM_CATEGORIES];

  typedef char fake_names[sizeof(char[2])]
  __attribute__ ((aligned(__alignof__(char[2]))));
  fake_names name_c[6 + _GLIBCXX_NUM_CATEGORIES];

  typedef char fake_facet_vec[sizeof(locale::facet*)]
  __attribute__ ((aligned(__alignof__(locale::facet*))));
  fake_facet_vec facet_vec[_GLIBCXX_NUM_FACETS];

  typedef char fake_cache_vec[sizeof(locale::facet*)]
  __attribute__ ((aligned(__alignof__(locale::facet*))));
  fake_cache_vec cache_vec[_GLIBCXX_NUM_FACETS];

  typedef char fake_ctype_c[sizeof(std::ctype<char>)]
  __attribute__ ((aligned(__alignof__(std::ctype<char>))));
  fake_ctype_c ctype_c;

  typedef char fake_collate_c[sizeof(std::collate<char>)]
  __attribute__ ((aligned(__alignof__(std::collate<char>))));
  fake_collate_c collate_c;

  typedef char fake_numpunct_c[sizeof(numpunct<char>)]
  __attribute__ ((aligned(__alignof__(numpunct<char>))));
  fake_numpunct_c numpunct_c;

  typedef char fake_num_get_c[sizeof(num_get<char>)]
  __attribute__ ((aligned(__alignof__(num_get<char>))));
  fake_num_get_c num_get_c;

  typedef char fake_num_put_c[sizeof(num_put<char>)]
  __attribute__ ((aligned(__alignof__(num_put<char>))));
  fake_num_put_c num_put_c;

  typedef char fake_codecvt_c[sizeof(codecvt<char, char, mbstate_t>)]
  __attribute__ ((aligned(__alignof__(codecvt<char, char, mbstate_t>))));
  fake_codecvt_c codecvt_c;

  typedef char fake_moneypunct_c[sizeof(moneypunct<char, true>)]
  __attribute__ ((aligned(__alignof__(moneypunct<char, true>))));
  fake_moneypunct_c moneypunct_ct;
  fake_moneypunct_c moneypunct_cf;

  typedef char fake_money_get_c[sizeof(money_get<char>)]
  __attribute__ ((aligned(__alignof__(money_get<char>))));
  fake_money_get_c money_get_c;
  
  typedef char fake_money_put_c[sizeof(money_put<char>)]
  __attribute__ ((aligned(__alignof__(money_put<char>))));
  fake_money_put_c money_put_c;

  typedef char fake_timepunct_c[sizeof(__timepunct<char>)]
  __attribute__ ((aligned(__alignof__(__timepunct<char>))));
  fake_timepunct_c timepunct_c;

  typedef char fake_time_get_c[sizeof(time_get<char>)]
  __attribute__ ((aligned(__alignof__(time_get<char>))));
  fake_time_get_c time_get_c;

  typedef char fake_time_put_c[sizeof(time_put<char>)]
  __attribute__ ((aligned(__alignof__(time_put<char>))));
  fake_time_put_c time_put_c;

  typedef char fake_messages_c[sizeof(messages<char>)]
  __attribute__ ((aligned(__alignof__(messages<char>))));
  fake_messages_c messages_c;

#ifdef  _GLIBCXX_USE_WCHAR_T
  typedef char fake_wtype_w[sizeof(std::ctype<wchar_t>)]
  __attribute__ ((aligned(__alignof__(std::ctype<wchar_t>))));
  fake_wtype_w ctype_w;

  typedef char fake_wollate_w[sizeof(std::collate<wchar_t>)]
  __attribute__ ((aligned(__alignof__(std::collate<wchar_t>))));
  fake_wollate_w collate_w;

  typedef char fake_numpunct_w[sizeof(numpunct<wchar_t>)]
  __attribute__ ((aligned(__alignof__(numpunct<wchar_t>))));
  fake_numpunct_w numpunct_w;

  typedef char fake_num_get_w[sizeof(num_get<wchar_t>)]
  __attribute__ ((aligned(__alignof__(num_get<wchar_t>))));
  fake_num_get_w num_get_w;

  typedef char fake_num_put_w[sizeof(num_put<wchar_t>)]
  __attribute__ ((aligned(__alignof__(num_put<wchar_t>))));
  fake_num_put_w num_put_w;

  typedef char fake_wodecvt_w[sizeof(codecvt<wchar_t, char, mbstate_t>)]
  __attribute__ ((aligned(__alignof__(codecvt<wchar_t, char, mbstate_t>))));
  fake_wodecvt_w codecvt_w;

  typedef char fake_moneypunct_w[sizeof(moneypunct<wchar_t, true>)]
  __attribute__ ((aligned(__alignof__(moneypunct<wchar_t, true>))));
  fake_moneypunct_w moneypunct_wt;
  fake_moneypunct_w moneypunct_wf;

  typedef char fake_money_get_w[sizeof(money_get<wchar_t>)]
  __attribute__ ((aligned(__alignof__(money_get<wchar_t>))));
  fake_money_get_w money_get_w;
  
  typedef char fake_money_put_w[sizeof(money_put<wchar_t>)]
  __attribute__ ((aligned(__alignof__(money_put<wchar_t>))));
  fake_money_put_w money_put_w;

  typedef char fake_timepunct_w[sizeof(__timepunct<wchar_t>)]
  __attribute__ ((aligned(__alignof__(__timepunct<wchar_t>))));
  fake_timepunct_w timepunct_w;

  typedef char fake_time_get_w[sizeof(time_get<wchar_t>)]
  __attribute__ ((aligned(__alignof__(time_get<wchar_t>))));
  fake_time_get_w time_get_w;

  typedef char fake_time_put_w[sizeof(time_put<wchar_t>)]
  __attribute__ ((aligned(__alignof__(time_put<wchar_t>))));
  fake_time_put_w time_put_w;

  typedef char fake_messages_w[sizeof(messages<wchar_t>)]
  __attribute__ ((aligned(__alignof__(messages<wchar_t>))));
  fake_messages_w messages_w;
#endif

  // Storage for "C" locale caches.
  typedef char fake_num_cache_c[sizeof(std::__numpunct_cache<char>)]
  __attribute__ ((aligned(__alignof__(std::__numpunct_cache<char>))));
  fake_num_cache_c numpunct_cache_c;

  typedef char fake_money_cache_c[sizeof(std::__moneypunct_cache<char, true>)]
  __attribute__ ((aligned(__alignof__(std::__moneypunct_cache<char, true>))));
  fake_money_cache_c moneypunct_cache_ct;
  fake_money_cache_c moneypunct_cache_cf;

  typedef char fake_time_cache_c[sizeof(std::__timepunct_cache<char>)]
  __attribute__ ((aligned(__alignof__(std::__timepunct_cache<char>))));
  fake_time_cache_c timepunct_cache_c;

#ifdef _GLIBCXX_USE_WCHAR_T
  typedef char fake_num_cache_w[sizeof(std::__numpunct_cache<wchar_t>)]
  __attribute__ ((aligned(__alignof__(std::__numpunct_cache<wchar_t>))));
  fake_num_cache_w numpunct_cache_w;

  typedef char fake_money_cache_w[sizeof(std::__moneypunct_cache<wchar_t,true>)]
  __attribute__ ((aligned(__alignof__(std::__moneypunct_cache<wchar_t,true>))));
  fake_money_cache_w moneypunct_cache_wt;
  fake_money_cache_w moneypunct_cache_wf;

  typedef char fake_time_cache_w[sizeof(std::__timepunct_cache<wchar_t>)]
  __attribute__ ((aligned(__alignof__(std::__timepunct_cache<wchar_t>))));
  fake_time_cache_w timepunct_cache_w;
#endif
} // namespace __gnu_internal
