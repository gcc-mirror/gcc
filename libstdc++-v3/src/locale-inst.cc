// Locale support -*- C++ -*-

// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

//
// ISO C++ 14882: 22.1  Locales
//

#include <bits/std_clocale.h>
#include <bits/std_cstring.h>
#include <bits/std_cassert.h>
#include <bits/std_limits.h>
#include <bits/std_exception.h>
#include <bits/std_stdexcept.h>
#include <bits/std_locale.h>
#include <bits/locale_facets.tcc>
#include <bits/std_istream.h>
#include <bits/std_ostream.h>

namespace std {

  typedef ostreambuf_iterator<char, char_traits<char> > obuf_iterator;
  typedef istreambuf_iterator<char, char_traits<char> > ibuf_iterator;
  typedef ostreambuf_iterator<wchar_t, char_traits<wchar_t> > wobuf_iterator;
  typedef istreambuf_iterator<wchar_t, char_traits<wchar_t> > wibuf_iterator;

  // moneypunct, money_get, and money_put

  const money_base::pattern
    money_base::__default_pattern = {{symbol, sign, none, value}};

  template class moneypunct<char, false>;
  template class moneypunct<char, true>;
  template class moneypunct_byname<char, false>;
  template class moneypunct_byname<char, true>;
  template class _Moneypunct<char>;
  template class money_get<char, obuf_iterator>;
  template class money_put<char, obuf_iterator>;
  template class money_get<char, ibuf_iterator>;
  template class money_put<char, ibuf_iterator>;
  template class _Format_cache<char>;

#ifdef _GLIBCPP_USE_WCHAR_T
  template class moneypunct<wchar_t, false>;
  template class moneypunct<wchar_t, true>;
  template class moneypunct_byname<wchar_t, false>;
  template class moneypunct_byname<wchar_t, true>;
  template class _Moneypunct<wchar_t>;
  template class money_get<wchar_t, wobuf_iterator>;
  template class money_put<wchar_t, wobuf_iterator>;
  template class money_get<wchar_t, wibuf_iterator>;
  template class money_put<wchar_t, wibuf_iterator>;
  template class _Format_cache<wchar_t>;
#endif

  // numpunct, numpunct_byname, num_get, and num_put
  template class numpunct<char>;
  template class numpunct_byname<char>;
  template class _Numpunct<char>;
  template class num_get<char, ibuf_iterator>;
  template class num_put<char, obuf_iterator>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class numpunct<wchar_t>;
  template class numpunct_byname<wchar_t>;
  template class _Numpunct<wchar_t>;
  template class num_get<wchar_t, wibuf_iterator>;
  template class num_put<wchar_t, wobuf_iterator>;
#endif

  // _Punct
  template class _Punct<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class _Punct<wchar_t>;
#endif
  
  // time_get and time_put
  //template class time_get<char, obuf_iterator>;
  template class time_put<char, obuf_iterator>;
  template class time_get<char, ibuf_iterator>;
  template class time_put<char, ibuf_iterator>;
#ifdef _GLIBCPP_USE_WCHAR_T
  //template class time_get<wchar_t, wobuf_iterator>;
  template class time_put<wchar_t, wobuf_iterator>;
  template class time_get<wchar_t, wibuf_iterator>;
  template class time_put<wchar_t, wibuf_iterator>;
#endif

  // messages
  template class _Messages<char>;
  template class messages<char>;
  template class messages_byname<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class _Messages<wchar_t>;
  template class messages<wchar_t>;
  template class messages_byname<wchar_t>;
#endif
  
  // ctype
  //  template class ctype<unsigned char>; // No definitions avail.
  //  template class ctype<signed char>; // No definitions avail.
  template class _Ctype<char>;
  template class _Ctype_nois<char>;
  template class ctype_byname<char>;
  template class _Codecvt<char, char, mbstate_t>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class _Ctype<wchar_t>;
  template class _Ctype_nois<wchar_t>;
  template class ctype_byname<wchar_t>;
  template class _Codecvt<wchar_t, char, mbstate_t>;
#endif
  
  // collate
  template class _Collate<char>;
  template class collate_byname<char>;
  template class _Weekdaynames<char, int>;
  template class _Monthnames<char, int>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class _Collate<wchar_t>;
  template class collate_byname<wchar_t>;
  template class _Weekdaynames<wchar_t, int>;
  template class _Monthnames<wchar_t, int>;
#endif
    
  // use_facet
  template 
    const num_put<char, obuf_iterator >& 
    use_facet<num_put<char, obuf_iterator> >(const locale &);
  template 
    const num_get<char, ibuf_iterator >& 
    use_facet<num_get<char, ibuf_iterator> >(const locale &);
  template
    const ctype<char>&
    use_facet<ctype<char> >(const locale& __loc);
  template
    const codecvt<char, char, mbstate_t>& 
    use_facet<codecvt<char, char, mbstate_t> >(locale const &);
  template 
    const num_put<char, obuf_iterator>& 
    _Use_facet_failure_handler<num_put<char, obuf_iterator> >
    (const locale &);
#ifdef _GLIBCPP_USE_WCHAR_T
  template 
    const num_put<wchar_t, wobuf_iterator>& 
    use_facet<num_put<wchar_t, wobuf_iterator> >(const locale &);
  template 
    const num_get<wchar_t, wibuf_iterator>& 
    use_facet<num_get<wchar_t, wibuf_iterator> >(const locale &);
  template
    const ctype<wchar_t>&
    use_facet<ctype<wchar_t> >(const locale& __loc);
  template
    const codecvt<wchar_t, wchar_t, mbstate_t>& 
    use_facet<codecvt<wchar_t, wchar_t, mbstate_t> >(locale const &);
  template
    const codecvt<wchar_t, char, mbstate_t>& 
    use_facet<codecvt<wchar_t, char, mbstate_t> >(locale const &);
  template 
    const num_put<wchar_t, wobuf_iterator>& 
    _Use_facet_failure_handler<num_put<wchar_t, wobuf_iterator> >
    (const locale &);
#endif

  // has_facet
  template 
    bool
    has_facet<numpunct<char> >(const locale &);
#ifdef _GLIBCPP_USE_WCHAR_T
  template 
    bool
    has_facet<numpunct<wchar_t> >(const locale &);
#endif

  //
  // iterator
  //
  typedef vector<locale::facet*> vec_pfacet;
  template 
    void 
    vec_pfacet::
    insert(vec_pfacet::iterator, vec_pfacet::size_type, 
	   const vec_pfacet::value_type&);

  typedef istreambuf_iterator<char, char_traits<char> > istreambuf_iter;
  typedef ostreambuf_iterator<char, char_traits<char> > ostreambuf_iter;


  template 
    istreambuf_iter 
    __match_parallel<istreambuf_iter, char>
    (istreambuf_iter, istreambuf_iter, int, const string*, int*, int&, bool&);


#ifdef _GLIBCPP_USE_WCHAR_T
  typedef istreambuf_iterator<wchar_t,char_traits<wchar_t> > wistreambuf_iter;
  typedef ostreambuf_iterator<wchar_t,char_traits<wchar_t> > wostreambuf_iter;

  template 
    wistreambuf_iter 
    __match_parallel<wistreambuf_iter, wchar_t>
   (wistreambuf_iter, wistreambuf_iter, int, const wstring*, int*, int&, bool&);
#endif


  //
  // locale
  //
  template 
    bool
    locale::operator()(const string&, const string&) const;

  template
    ostreambuf_iter
    _S_fill<char, ostreambuf_iter, output_iterator_tag>
    (ostreambuf_iter, char, int, output_iterator_tag);

  template 
    ostreambuf_iter
    _S_pad_numeric<char, ostreambuf_iter>
    (ostreambuf_iter, ios_base::fmtflags, char, int, char const*, char const*, 
     char const*);

  template
    char*
    _S_group_digits<char>(char*, char, char const*, char const*, 
			  char const*, char const*);

  template 
    ostreambuf_iter
    _S_format<char, ostreambuf_iter, unsigned long>
    (ostreambuf_iter, ios_base &, char, bool, unsigned long);

#ifdef _GLIBCPP_USE_LONG_LONG
  template
    ostreambuf_iter
    _S_format<char, ostreambuf_iter, unsigned long long>
    (ostreambuf_iter, ios_base &, char, bool, unsigned long long);
#endif

#ifdef _GLIBCPP_USE_WCHAR_T
  template 
    bool
    locale::operator()(const wstring&, const wstring&) const;

  typedef ostreambuf_iterator<wchar_t> wostreambuf_iter;

  template
    wostreambuf_iter
    _S_fill<wchar_t, wostreambuf_iter, output_iterator_tag>
    (wostreambuf_iter, wchar_t, int, output_iterator_tag);

  template 
    wostreambuf_iter
    _S_pad_numeric<wchar_t, wostreambuf_iter>
    (wostreambuf_iter, ios_base::fmtflags, wchar_t __fill, int, wchar_t const*, 
     wchar_t const*, wchar_t const*);

  template
    wchar_t*
    _S_group_digits<wchar_t>(wchar_t*, wchar_t, char const*, char const*, 
			     wchar_t const*, wchar_t const*);

  template 
    wostreambuf_iter
    _S_format<wchar_t, wostreambuf_iter, unsigned long>
    (wostreambuf_iter, ios_base &, wchar_t, bool, unsigned long);

#ifdef _GLIBCPP_USE_LONG_LONG
  template
    wostreambuf_iter
    _S_format<wchar_t, wostreambuf_iter, unsigned long long>
    (wostreambuf_iter, ios_base &, wchar_t, bool, unsigned long long);
#endif
#endif // _GLIBCPP_USE_WCHAR_T

  template 
    locale::facet** 
    fill_n<locale::facet**, unsigned int, locale::facet*>
    (locale::facet**, unsigned int, locale::facet* const &);

} //std






