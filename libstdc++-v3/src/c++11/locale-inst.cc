// Locale support -*- C++ -*-

// Copyright (C) 1999-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

//
// ISO C++ 14882: 22.1  Locales
//

#ifndef _GLIBCXX_USE_CXX11_ABI
// Instantiations in this file use the old COW std::string ABI unless included
// by another file which defines _GLIBCXX_USE_CXX11_ABI=1. Some instantiations
// are guarded by a check for !_GLIBCXX_USE_CXX11_ABI so that they are only
// instantiated once, because they are not tagged with abi_tag so should not
// be instantiated twice.
# define _GLIBCXX_USE_CXX11_ABI 0
#endif

#include <locale>

// Instantiation configuration.
#ifndef C
# define C char
# define C_is_char
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // moneypunct, money_get, and money_put
#if ! _GLIBCXX_USE_CXX11_ABI
  template struct __moneypunct_cache<C, false>;
  template struct __moneypunct_cache<C, true>;
#endif
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class moneypunct<C, false>;
  template class moneypunct<C, true>;
  template class moneypunct_byname<C, false>;
  template class moneypunct_byname<C, true>;
_GLIBCXX_END_NAMESPACE_CXX11
_GLIBCXX_BEGIN_NAMESPACE_LDBL_OR_CXX11
  template class money_get<C, istreambuf_iterator<C> >;
  template class money_put<C, ostreambuf_iterator<C> >;
  template
    istreambuf_iterator<C>
    money_get<C, istreambuf_iterator<C> >::
    _M_extract<true>(istreambuf_iterator<C>, istreambuf_iterator<C>,
		     ios_base&, ios_base::iostate&, string&) const;

  template
    istreambuf_iterator<C>
    money_get<C, istreambuf_iterator<C> >::
    _M_extract<false>(istreambuf_iterator<C>, istreambuf_iterator<C>,
		      ios_base&, ios_base::iostate&, string&) const;

  template
    ostreambuf_iterator<C>
    money_put<C, ostreambuf_iterator<C> >::
    _M_insert<true>(ostreambuf_iterator<C>, ios_base&, C,
		    const string_type&) const;

  template
    ostreambuf_iterator<C>
    money_put<C, ostreambuf_iterator<C> >::
    _M_insert<false>(ostreambuf_iterator<C>, ios_base&, C,
		     const string_type&) const;
_GLIBCXX_END_NAMESPACE_LDBL_OR_CXX11

  // numpunct, numpunct_byname, num_get, and num_put
#if ! _GLIBCXX_USE_CXX11_ABI
  template struct __numpunct_cache<C>;
#endif
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class numpunct<C>;
  template class numpunct_byname<C>;
_GLIBCXX_END_NAMESPACE_CXX11
_GLIBCXX_BEGIN_NAMESPACE_LDBL
#if ! _GLIBCXX_USE_CXX11_ABI
  template class num_get<C, istreambuf_iterator<C> >;
#endif

  template
    istreambuf_iterator<C>
    num_get<C, istreambuf_iterator<C> >::
    _M_extract_int(istreambuf_iterator<C>, istreambuf_iterator<C>,
		   ios_base&, ios_base::iostate&,
		   long&) const;

  template
    istreambuf_iterator<C>
    num_get<C, istreambuf_iterator<C> >::
    _M_extract_int(istreambuf_iterator<C>, istreambuf_iterator<C>,
		   ios_base&, ios_base::iostate&,
		   unsigned short&) const;

  template
    istreambuf_iterator<C>
    num_get<C, istreambuf_iterator<C> >::
    _M_extract_int(istreambuf_iterator<C>, istreambuf_iterator<C>,
		   ios_base&, ios_base::iostate&,
		   unsigned int&) const;

  template
    istreambuf_iterator<C>
    num_get<C, istreambuf_iterator<C> >::
    _M_extract_int(istreambuf_iterator<C>, istreambuf_iterator<C>,
		   ios_base&, ios_base::iostate&,
		   unsigned long&) const;

#ifdef _GLIBCXX_USE_LONG_LONG
  template
    istreambuf_iterator<C>
    num_get<C, istreambuf_iterator<C> >::
    _M_extract_int(istreambuf_iterator<C>, istreambuf_iterator<C>,
		   ios_base&, ios_base::iostate&,
		   long long&) const;

  template
    istreambuf_iterator<C>
    num_get<C, istreambuf_iterator<C> >::
    _M_extract_int(istreambuf_iterator<C>, istreambuf_iterator<C>,
		   ios_base&, ios_base::iostate&,
		   unsigned long long&) const;
#endif

#if ! _GLIBCXX_USE_CXX11_ABI
  template class num_put<C, ostreambuf_iterator<C> >;

  template
    ostreambuf_iterator<C>
    num_put<C, ostreambuf_iterator<C> >::
    _M_insert_int(ostreambuf_iterator<C>, ios_base&, C,
		  long) const;

  template
    ostreambuf_iterator<C>
    num_put<C, ostreambuf_iterator<C> >::
    _M_insert_int(ostreambuf_iterator<C>, ios_base&, C,
		  unsigned long) const;

#ifdef _GLIBCXX_USE_LONG_LONG
  template
    ostreambuf_iterator<C>
    num_put<C, ostreambuf_iterator<C> >::
    _M_insert_int(ostreambuf_iterator<C>, ios_base&, C,
		  long long) const;

  template
    ostreambuf_iterator<C>
    num_put<C, ostreambuf_iterator<C> >::
    _M_insert_int(ostreambuf_iterator<C>, ios_base&, C,
		  unsigned long long) const;
#endif

  template
    ostreambuf_iterator<C>
    num_put<C, ostreambuf_iterator<C> >::
    _M_insert_float(ostreambuf_iterator<C>, ios_base&, C, char,
		    double) const;

  template
    ostreambuf_iterator<C>
    num_put<C, ostreambuf_iterator<C> >::
    _M_insert_float(ostreambuf_iterator<C>, ios_base&, C, char,
		    long double) const;
#endif
_GLIBCXX_END_NAMESPACE_LDBL

  // time_get and time_put
#if ! _GLIBCXX_USE_CXX11_ABI
  template class __timepunct<C>;
  template struct __timepunct_cache<C>;
  template class time_put<C, ostreambuf_iterator<C> >;
  template class time_put_byname<C, ostreambuf_iterator<C> >;
#else
  // Instantiate constructor taking __cxx11::string
  template time_put_byname<C>::time_put_byname(const string&, size_t);
#endif
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class time_get<C, istreambuf_iterator<C> >;
  template class time_get_byname<C, istreambuf_iterator<C> >;
_GLIBCXX_END_NAMESPACE_CXX11

  // messages
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class messages<C>;
  template class messages_byname<C>;
_GLIBCXX_END_NAMESPACE_CXX11

  // ctype
  ctype_byname<C>::ctype_byname(const string& __s, size_t __refs)
  : ctype_byname(__s.c_str(), __refs) { }

#if ! _GLIBCXX_USE_CXX11_ABI
  inline template class __ctype_abstract_base<C>;
  template class ctype_byname<C>;
#endif

  // codecvt
#if ! _GLIBCXX_USE_CXX11_ABI
  inline template class __codecvt_abstract_base<C, char, mbstate_t>;
  template class codecvt_byname<C, char, mbstate_t>;
#else
  // Instantiate constructor taking __cxx11::string
  template codecvt_byname<C, char, mbstate_t>::codecvt_byname(const string&, size_t);
#endif

  // collate
_GLIBCXX_BEGIN_NAMESPACE_CXX11
  template class collate<C>;
  template class collate_byname<C>;
_GLIBCXX_END_NAMESPACE_CXX11

  // use_facet
#if ! _GLIBCXX_USE_CXX11_ABI
  template
    const ctype<C>&
    use_facet<ctype<C> >(const locale&);

  template
    const codecvt<C, char, mbstate_t>&
    use_facet<codecvt<C, char, mbstate_t> >(const locale&);
#endif

  template
    const collate<C>&
    use_facet<collate<C> >(const locale&);

  template
    const numpunct<C>&
    use_facet<numpunct<C> >(const locale&);

#if ! _GLIBCXX_USE_CXX11_ABI
  template
    const num_put<C>&
    use_facet<num_put<C> >(const locale&);

  template
    const num_get<C>&
    use_facet<num_get<C> >(const locale&);
#endif

  template
    const moneypunct<C, true>&
    use_facet<moneypunct<C, true> >(const locale&);

  template
    const moneypunct<C, false>&
    use_facet<moneypunct<C, false> >(const locale&);

  template
    const money_put<C>&
    use_facet<money_put<C> >(const locale&);

  template
    const money_get<C>&
    use_facet<money_get<C> >(const locale&);

#if ! _GLIBCXX_USE_CXX11_ABI
  template
    const __timepunct<C>&
    use_facet<__timepunct<C> >(const locale&);

  template
    const time_put<C>&
    use_facet<time_put<C> >(const locale&);
#endif

  template
    const time_get<C>&
    use_facet<time_get<C> >(const locale&);

  template
    const messages<C>&
    use_facet<messages<C> >(const locale&);

  // has_facet
#if ! _GLIBCXX_USE_CXX11_ABI
  template
    bool
    has_facet<ctype<C> >(const locale&);

  template
    bool
    has_facet<codecvt<C, char, mbstate_t> >(const locale&);
#endif

  template
    bool
    has_facet<collate<C> >(const locale&);

  template
    bool
    has_facet<numpunct<C> >(const locale&);

#if ! _GLIBCXX_USE_CXX11_ABI
  template
    bool
    has_facet<num_put<C> >(const locale&);

  template
    bool
    has_facet<num_get<C> >(const locale&);
#endif

  template
    bool
    has_facet<moneypunct<C> >(const locale&);

  template
    bool
    has_facet<money_put<C> >(const locale&);

  template
    bool
    has_facet<money_get<C> >(const locale&);

#if ! _GLIBCXX_USE_CXX11_ABI
  template
    bool
    has_facet<__timepunct<C> >(const locale&);

  template
    bool
    has_facet<time_put<C> >(const locale&);
#endif

  template
    bool
    has_facet<time_get<C> >(const locale&);

  template
    bool
    has_facet<messages<C> >(const locale&);


#if ! _GLIBCXX_USE_CXX11_ABI
  // locale functions.
  template
    C*
    __add_grouping<C>(C*, C, char const*, size_t,
			 C const*, C const*);

  template class __pad<C, char_traits<C> >;

  template
    int
    __int_to_char(C*, unsigned long, const C*,
		  ios_base::fmtflags, bool);

#ifdef _GLIBCXX_USE_LONG_LONG
  template
    int
    __int_to_char(C*, unsigned long long, const C*,
		  ios_base::fmtflags, bool);
#endif
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// XXX GLIBCXX_ABI Deprecated
#if defined _GLIBCXX_LONG_DOUBLE_COMPAT && defined C_is_char \
      && _GLIBCXX_USE_CXX11_ABI == 0

#define _GLIBCXX_LDBL_COMPAT(dbl, ldbl) \
  extern "C" void ldbl (void) __attribute__ ((alias (#dbl), weak))

_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intIjEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intIjEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intIlEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intIlEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intImEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intImEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intItEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intItEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intIxEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intIxEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intIyEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_,
		     _ZNKSt7num_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE14_M_extract_intIyEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE13_M_insert_intIlEES4_S4_RSt8ios_basecT_,
		     _ZNKSt7num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE13_M_insert_intIlEES3_S3_RSt8ios_basecT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE13_M_insert_intImEES4_S4_RSt8ios_basecT_,
		     _ZNKSt7num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE13_M_insert_intImEES3_S3_RSt8ios_basecT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE13_M_insert_intIxEES4_S4_RSt8ios_basecT_,
		     _ZNKSt7num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE13_M_insert_intIxEES3_S3_RSt8ios_basecT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE13_M_insert_intIyEES4_S4_RSt8ios_basecT_,
		     _ZNKSt7num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE13_M_insert_intIyEES3_S3_RSt8ios_basecT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1287num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE15_M_insert_floatIdEES4_S4_RSt8ios_baseccT_,
		     _ZNKSt7num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE15_M_insert_floatIdEES3_S3_RSt8ios_baseccT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt7num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE15_M_insert_floatIdEES3_S3_RSt8ios_baseccT_,
		     _ZNKSt7num_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE15_M_insert_floatIeEES3_S3_RSt8ios_baseccT_);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1289money_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE10_M_extractILb0EEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRSs,
		     _ZNKSt9money_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE10_M_extractILb0EEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRSs);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1289money_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE10_M_extractILb1EEES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRSs,
		     _ZNKSt9money_getIcSt19istreambuf_iteratorIcSt11char_traitsIcEEE10_M_extractILb1EEES3_S3_S3_RSt8ios_baseRSt12_Ios_IostateRSs);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1289money_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE9_M_insertILb0EEES4_S4_RSt8ios_basecRKSs,
		     _ZNKSt9money_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE9_M_insertILb0EEES3_S3_RSt8ios_basecRKSs);
_GLIBCXX_LDBL_COMPAT(_ZNKSt17__gnu_cxx_ldbl1289money_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE9_M_insertILb1EEES4_S4_RSt8ios_basecRKSs,
		     _ZNKSt9money_putIcSt19ostreambuf_iteratorIcSt11char_traitsIcEEE9_M_insertILb1EEES3_S3_RSt8ios_basecRKSs);

#endif // _GLIBCXX_LONG_DOUBLE_COMPAT
