// Locale support -*- C++ -*-

// Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
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

#include <cstdlib>
#include <clocale>
#include <cstring>
#include <cassert>
#include <limits>
#include <exception>
#include <stdexcept>
#include <locale>
#include <istream>
#include <ostream>

namespace std
{
  typedef ostreambuf_iterator<char, char_traits<char> > obuf_iterator;
  typedef istreambuf_iterator<char, char_traits<char> > ibuf_iterator;
  typedef ostreambuf_iterator<wchar_t, char_traits<wchar_t> > wobuf_iterator;
  typedef istreambuf_iterator<wchar_t, char_traits<wchar_t> > wibuf_iterator;

  // moneypunct, money_get, and money_put
  template class moneypunct<char, false>;
  template class moneypunct<char, true>;
  template class moneypunct_byname<char, false>;
  template class moneypunct_byname<char, true>;
  template class money_get<char, ibuf_iterator>;
  template class money_put<char, obuf_iterator>;

#ifdef _GLIBCPP_USE_WCHAR_T
  template class moneypunct<wchar_t, false>;
  template class moneypunct<wchar_t, true>;
  template class moneypunct_byname<wchar_t, false>;
  template class moneypunct_byname<wchar_t, true>;
  template class money_get<wchar_t, wibuf_iterator>;
  template class money_put<wchar_t, wobuf_iterator>;
#endif

  // numpunct, numpunct_byname, num_get, and num_put
  template class numpunct<char>;
  template class numpunct_byname<char>;
  template class num_get<char, ibuf_iterator>;
  template class num_put<char, obuf_iterator>; 
  template
    obuf_iterator
    num_put<char, obuf_iterator>::
    _M_convert_int(obuf_iterator, ios_base&, char, char, char, long) const;

  template
    obuf_iterator
    num_put<char, obuf_iterator>::
    _M_convert_int(obuf_iterator, ios_base&, char, char, char, 
		   unsigned long) const;

#ifdef _GLIBCPP_USE_LONG_LONG
  template
    obuf_iterator
    num_put<char, obuf_iterator>::
    _M_convert_int(obuf_iterator, ios_base&, char, char, char, 
		   long long) const;

  template
    obuf_iterator
    num_put<char, obuf_iterator>::
    _M_convert_int(obuf_iterator, ios_base&, char, char, char,
		   unsigned long long) const;
#endif

  template
    obuf_iterator
    num_put<char, obuf_iterator>::
    _M_convert_float(obuf_iterator, ios_base&, char, char, double) const;

  template
    obuf_iterator
    num_put<char, obuf_iterator>::
    _M_convert_float(obuf_iterator, ios_base&, char, char, 
		    long double) const;

#ifdef _GLIBCPP_USE_WCHAR_T
  template class numpunct<wchar_t>;
  template class numpunct_byname<wchar_t>;
  template class num_get<wchar_t, wibuf_iterator>;
  template class num_put<wchar_t, wobuf_iterator>;

  template
    wobuf_iterator
    num_put<wchar_t, wobuf_iterator>::
    _M_convert_int(wobuf_iterator, ios_base&, wchar_t, char, char, long) const;

  template
    wobuf_iterator
    num_put<wchar_t, wobuf_iterator>::
    _M_convert_int(wobuf_iterator, ios_base&, wchar_t, char, char,
		   unsigned long) const;

#ifdef _GLIBCPP_USE_LONG_LONG
  template
    wobuf_iterator
    num_put<wchar_t, wobuf_iterator>::
    _M_convert_int(wobuf_iterator, ios_base&, wchar_t, char, char,
		   long long) const;

  template
    wobuf_iterator
    num_put<wchar_t, wobuf_iterator>::
    _M_convert_int(wobuf_iterator, ios_base&, wchar_t, char, char,
		   unsigned long long) const;
#endif

  template
    wobuf_iterator
    num_put<wchar_t, wobuf_iterator>::
    _M_convert_float(wobuf_iterator, ios_base&, wchar_t, char, 
		     double) const;

  template
    wobuf_iterator
    num_put<wchar_t, wobuf_iterator>::
    _M_convert_float(wobuf_iterator, ios_base&, wchar_t, char, 
		     long double) const;
#endif

  // time_get and time_put
  template class __timepunct<char>;
  template class time_put<char, obuf_iterator>;
  template class time_put_byname<char, obuf_iterator>;
  template class time_get<char, ibuf_iterator>;
  template class time_get_byname<char, ibuf_iterator>;

#ifdef _GLIBCPP_USE_WCHAR_T
  template class __timepunct<wchar_t>;
  template class time_put<wchar_t, wobuf_iterator>;
  template class time_put_byname<wchar_t, wobuf_iterator>;
  template class time_get<wchar_t, wibuf_iterator>;
  template class time_get_byname<wchar_t, wibuf_iterator>;
#endif

  // messages
  template class messages<char>;
  template class messages_byname<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class messages<wchar_t>;
  template class messages_byname<wchar_t>;
#endif
  
  // ctype
  template class __ctype_abstract_base<char>;
  template class ctype_byname<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class __ctype_abstract_base<wchar_t>;
  template class ctype_byname<wchar_t>;
#endif
  
  // codecvt
  template class __codecvt_abstract_base<char, char, mbstate_t>;
  template class __codecvt_abstract_base<wchar_t, char, mbstate_t>;
  template class codecvt_byname<char, char, mbstate_t>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class codecvt_byname<wchar_t, char, mbstate_t>;
#endif

  // collate
  template class collate<char>;
  template class collate_byname<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class collate<wchar_t>;
  template class collate_byname<wchar_t>;
#endif
    
  // use_facet
  template
    const numpunct<char>& 
    use_facet<numpunct<char> >(const locale&);

  template 
    const num_put<char, obuf_iterator >& 
    use_facet<num_put<char, obuf_iterator> >(const locale&);

  template 
    const num_get<char, ibuf_iterator >& 
    use_facet<num_get<char, ibuf_iterator> >(const locale&);

  template
    const codecvt<char, char, mbstate_t>& 
    use_facet<codecvt<char, char, mbstate_t> >(const locale&);

  template
    const collate<char>& 
    use_facet<collate<char> >(const locale&);

  template
    const moneypunct<char, true>& 
    use_facet<moneypunct<char, true> >(const locale&);

  template
    const moneypunct<char, false>& 
    use_facet<moneypunct<char, false> >(const locale&);

  template
    const __timepunct<char>& 
    use_facet<__timepunct<char> >(const locale&);

#ifdef _GLIBCPP_USE_WCHAR_T
  template
    const numpunct<wchar_t>& 
    use_facet<numpunct<wchar_t> >(const locale&);

  template 
    const num_put<wchar_t, wobuf_iterator>& 
    use_facet<num_put<wchar_t, wobuf_iterator> >(const locale&);

  template 
    const num_get<wchar_t, wibuf_iterator>& 
    use_facet<num_get<wchar_t, wibuf_iterator> >(const locale&);

  template
    const codecvt<wchar_t, char, mbstate_t>& 
    use_facet<codecvt<wchar_t, char, mbstate_t> >(locale const&);

  template
    const collate<wchar_t>& 
    use_facet<collate<wchar_t> >(const locale&);

  template
    const moneypunct<wchar_t, true>& 
    use_facet<moneypunct<wchar_t, true> >(const locale&);

  template
    const moneypunct<wchar_t, false>& 
    use_facet<moneypunct<wchar_t, false> >(const locale&);

  template
    const __timepunct<wchar_t>& 
    use_facet<__timepunct<wchar_t> >(const locale&);
#endif

  // has_facet
  template 
    bool
    has_facet<numpunct<char> >(const locale&);
  template 
    bool
    has_facet<num_put<char> >(const locale&);
  template 
    bool
    has_facet<num_get<char> >(const locale&);
  template 
    bool
    has_facet<ctype<char> >(const locale&);

#ifdef _GLIBCPP_USE_WCHAR_T
  template 
    bool
    has_facet<numpunct<wchar_t> >(const locale&);
  template 
    bool
    has_facet<num_put<wchar_t> >(const locale&);
  template 
    bool
    has_facet<num_get<wchar_t> >(const locale&);
  template 
    bool
    has_facet<ctype<wchar_t> >(const locale&);
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
  template 
    void 
    vec_pfacet::
    _M_fill_insert(vec_pfacet::iterator, vec_pfacet::size_type, 
		   const vec_pfacet::value_type&);


  //
  // locale
  //
  typedef istreambuf_iterator<char, char_traits<char> > istreambuf_iter;
  typedef ostreambuf_iterator<char, char_traits<char> > ostreambuf_iter;

#ifdef _GLIBCPP_USE_WCHAR_T
  typedef istreambuf_iterator<wchar_t, char_traits<wchar_t> > wistreambuf_iter;
  typedef ostreambuf_iterator<wchar_t, char_traits<wchar_t> > wostreambuf_iter;
#endif

  template 
    bool
    locale::operator()(const string&, const string&) const;

  template
    char*
    __add_grouping<char>(char*, char, char const*, char const*, 
			 char const*, char const*);

  template
    bool
    __verify_grouping<char>(const basic_string<char>&, basic_string<char>&);

  template
    void 
    __pad<char>(ios_base&, char, char*, const char *, streamsize, 
		streamsize, const bool);

  template
    void 
    __pad<char, char_traits<char> >(ios_base&, char, char*, const char *, 
				    streamsize, streamsize, const bool);

#ifdef _GLIBCPP_USE_WCHAR_T
  template 
    bool
    locale::operator()(const wstring&, const wstring&) const;

  typedef ostreambuf_iterator<wchar_t> wostreambuf_iter;

  template
    wchar_t*
    __add_grouping<wchar_t>(wchar_t*, wchar_t, char const*, char const*, 
			    wchar_t const*, wchar_t const*);
  template
    bool
    __verify_grouping<wchar_t>(const basic_string<wchar_t>&, 
			       basic_string<wchar_t>&);

  template
    void 
    __pad<wchar_t>(ios_base&, wchar_t, wchar_t*, const wchar_t*, 
		   streamsize, streamsize, const bool);

  template
    void 
    __pad<wchar_t, char_traits<wchar_t> >(ios_base&, wchar_t, wchar_t*, 
					  const wchar_t*, streamsize, 
					  streamsize, const bool);
#endif // _GLIBCPP_USE_WCHAR_T

  template 
    locale::facet** 
    fill_n<locale::facet**, size_t, locale::facet*>
    (locale::facet**, size_t, locale::facet* const&);

  template
    __normal_iterator<locale::facet**, vector<locale::facet*> >
    fill_n(__normal_iterator<locale::facet**, vector<locale::facet*> >,
	   size_t, locale::facet* const&);

  template
    void
    fill(__normal_iterator<locale::facet**, vector<locale::facet*> >,
         __normal_iterator<locale::facet**, vector<locale::facet*> >,
         locale::facet* const&);
} // namespace std
