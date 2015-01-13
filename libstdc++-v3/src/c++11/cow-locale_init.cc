// Copyright (C) 2014-2015 Free Software Foundation, Inc.
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

// Instantiate the facets using old std::string ABI.
#define _GLIBCXX_USE_CXX11_ABI 0
#include <locale>

#if ! _GLIBCXX_USE_DUAL_ABI
# error This file should not be compiled for this configuration.
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace 
{
  typedef char fake_collate_c[sizeof(std::collate<char>)]
  __attribute__ ((aligned(__alignof__(std::collate<char>))));
  fake_collate_c collate_c;

  typedef char fake_numpunct_c[sizeof(numpunct<char>)]
  __attribute__ ((aligned(__alignof__(numpunct<char>))));
  fake_numpunct_c numpunct_c;

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

  typedef char fake_time_get_c[sizeof(time_get<char>)]
  __attribute__ ((aligned(__alignof__(time_get<char>))));
  fake_time_get_c time_get_c;

  typedef char fake_messages_c[sizeof(messages<char>)]
  __attribute__ ((aligned(__alignof__(messages<char>))));
  fake_messages_c messages_c;

#ifdef  _GLIBCXX_USE_WCHAR_T
  typedef char fake_wollate_w[sizeof(std::collate<wchar_t>)]
  __attribute__ ((aligned(__alignof__(std::collate<wchar_t>))));
  fake_wollate_w collate_w;

  typedef char fake_numpunct_w[sizeof(numpunct<wchar_t>)]
  __attribute__ ((aligned(__alignof__(numpunct<wchar_t>))));
  fake_numpunct_w numpunct_w;

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

  typedef char fake_time_get_w[sizeof(time_get<wchar_t>)]
  __attribute__ ((aligned(__alignof__(time_get<wchar_t>))));
  fake_time_get_w time_get_w;

  typedef char fake_messages_w[sizeof(messages<wchar_t>)]
  __attribute__ ((aligned(__alignof__(messages<wchar_t>))));
  fake_messages_w messages_w;
#endif
} // anonymous namespace

  void
  locale::_Impl::_M_init_extra(facet** caches)
  {
    auto __npc = static_cast<__numpunct_cache<char>*>(caches[0]);
    auto __mpcf = static_cast<__moneypunct_cache<char, false>*>(caches[1]);
    auto __mpct = static_cast<__moneypunct_cache<char, true>*>(caches[2]);

    _M_init_facet_unchecked(new (&numpunct_c) numpunct<char>(__npc, 1));
    _M_init_facet_unchecked(new (&collate_c) std::collate<char>(1));
    _M_init_facet_unchecked(new (&moneypunct_cf) moneypunct<char, false>(__mpcf, 1));
    _M_init_facet_unchecked(new (&moneypunct_ct) moneypunct<char, true>(__mpct, 1));
    _M_init_facet_unchecked(new (&money_get_c) money_get<char>(1));
    _M_init_facet_unchecked(new (&money_put_c) money_put<char>(1));
    _M_init_facet_unchecked(new (&time_get_c) time_get<char>(1));
    _M_init_facet_unchecked(new (&messages_c) std::messages<char>(1));
#ifdef _GLIBCXX_USE_WCHAR_T
    auto __npw = static_cast<__numpunct_cache<wchar_t>*>(caches[3]);
    auto __mpwf = static_cast<__moneypunct_cache<wchar_t, false>*>(caches[4]);
    auto __mpwt = static_cast<__moneypunct_cache<wchar_t, true>*>(caches[5]);

    _M_init_facet_unchecked(new (&numpunct_w) numpunct<wchar_t>(__npw, 1));
    _M_init_facet_unchecked(new (&collate_w) std::collate<wchar_t>(1));
    _M_init_facet_unchecked(new (&moneypunct_wf) moneypunct<wchar_t, false>(__mpwf, 1));
    _M_init_facet_unchecked(new (&moneypunct_wt) moneypunct<wchar_t, true>(__mpwt, 1));
    _M_init_facet_unchecked(new (&money_get_w) money_get<wchar_t>(1));
    _M_init_facet_unchecked(new (&money_put_w) money_put<wchar_t>(1));
    _M_init_facet_unchecked(new (&time_get_w) time_get<wchar_t>(1));
    _M_init_facet_unchecked(new (&messages_w) std::messages<wchar_t>(1));
#endif

    _M_caches[numpunct<char>::id._M_id()] = __npc;
    _M_caches[moneypunct<char, false>::id._M_id()] = __mpcf;
    _M_caches[moneypunct<char, true>::id._M_id()] = __mpct;
#ifdef  _GLIBCXX_USE_WCHAR_T
    _M_caches[numpunct<wchar_t>::id._M_id()] = __npw;
    _M_caches[moneypunct<wchar_t, false>::id._M_id()] = __mpwf;
    _M_caches[moneypunct<wchar_t, true>::id._M_id()] = __mpwt;
#endif
  }

  void
  locale::_Impl::_M_init_extra(void* cloc, void* clocm,
                               const char* __s, const char* __smon)
  {
    auto& __cloc = *static_cast<__c_locale*>(cloc);

    _M_init_facet_unchecked(new numpunct<char>(__cloc));
    _M_init_facet_unchecked(new std::collate<char>(__cloc));
    _M_init_facet_unchecked(new moneypunct<char, false>(__cloc, 0));
    _M_init_facet_unchecked(new moneypunct<char, true>(__cloc, 0));
    _M_init_facet_unchecked(new money_get<char>);
    _M_init_facet_unchecked(new money_put<char>);
    _M_init_facet_unchecked(new time_get<char>);
    _M_init_facet_unchecked(new std::messages<char>(__cloc, __s));

#ifdef  _GLIBCXX_USE_WCHAR_T
    auto& __clocm = *static_cast<__c_locale*>(clocm);

    _M_init_facet_unchecked(new numpunct<wchar_t>(__cloc));
    _M_init_facet_unchecked(new std::collate<wchar_t>(__cloc));
    _M_init_facet_unchecked(new moneypunct<wchar_t, false>(__clocm, __smon));
    _M_init_facet_unchecked(new moneypunct<wchar_t, true>(__clocm, __smon));
    _M_init_facet_unchecked(new money_get<wchar_t>);
    _M_init_facet_unchecked(new money_put<wchar_t>);
    _M_init_facet_unchecked(new time_get<wchar_t>);
    _M_init_facet_unchecked(new std::messages<wchar_t>(__cloc, __s));
#endif	  
  }

// TODO should be in another file
  string
  locale::name() const
  {
    string __ret;
    if (!_M_impl->_M_names[0])
      __ret = '*';
    else if (_M_impl->_M_check_same_name())
      __ret = _M_impl->_M_names[0];
    else
      {
	__ret.reserve(128);
	__ret += _S_categories[0];
	__ret += '=';
	__ret += _M_impl->_M_names[0]; 
	for (size_t __i = 1; __i < _S_categories_size; ++__i)
	  {
	    __ret += ';';
	    __ret += _S_categories[__i];
	    __ret += '=';
	    __ret += _M_impl->_M_names[__i];
	  }
      }
    return __ret;
  }

_GLIBCXX_END_NAMESPACE_VERSION
}
