// Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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


#include <bits/std_clocale.h>
#include <bits/std_locale.h>
#include <bits/std_cstring.h>
#include <bits/std_vector.h>
#include <bits/std_stdexcept.h>

namespace std {

  locale::_Impl::
  ~_Impl() throw()
  {
    std::vector<facet*>::iterator it = _M_facets->begin();
    for (; it != _M_facets->end(); ++it)
      (*it)->_M_remove_reference();
    delete _M_facets;
    delete _M_category_names;
  }

  locale::_Impl::
  _Impl(const _Impl& __other, size_t __refs)
  : _M_references(__refs - 1), _M_facets(0), _M_category_names(0), 
    _M_has_name(__other._M_has_name), _M_name(__other._M_name)
  {
    try
      {  _M_facets = new __vec_facet(*(__other._M_facets)); }
    catch(...) 
      {
	delete _M_facets;
	throw;
      }

    try 
      {	_M_category_names = new __vec_string(*(__other._M_category_names)); }
    catch(...) 
      {
	delete _M_category_names;
	throw;
      }

    std::vector<facet*>::iterator __it = _M_facets->begin();
    for (; __it != _M_facets->end(); ++__it)
      (*__it)->_M_add_reference();
  }

  // This constructor is used to correctly initialize named locales,
  // including the standard "C" locale.
  locale::_Impl::
  _Impl(size_t __numfacets, size_t __refs, bool __has_name = false, 
	string __name)
  : _M_references(__refs - 1), _M_facets(0), _M_category_names(0), 
    _M_has_name(__has_name), _M_name(__name)
  {
    try
      {  _M_facets = new __vec_facet(__numfacets, NULL); }
    catch(...) 
      {
	delete _M_facets;
	throw;
      }

    try 
      {	_M_category_names = new __vec_string(_S_categories_num, _M_name); }
    catch(...) 
      {
	delete _M_category_names;
	throw;
      }
  }
  
  // Construct specific categories, leaving unselected ones alone
  locale::_Impl::
  _Impl(const _Impl& __other, const string& __name, category __cat, 
	size_t __refs)
    : _M_references(__refs - 1), _M_has_name(__other._M_name != "*")
  {
    __cat = _S_normalize_category(__cat);  // might throw

    try 
      { _M_facets = new __vec_facet(*(__other._M_facets)); }
    catch(...) 
      {
	delete _M_facets;
	throw;
      }

    try 
      {	_M_category_names = new __vec_string(*(__other._M_category_names)); }
    catch(...) 
      {
	delete _M_category_names;
	throw;
      }

    static void(_Impl::* ctors[]) (const char*) = 
    {
      //  NB: Order must match the decl order in class locale.
      &locale::_Impl::_M_construct_ctype,
      &locale::_Impl::_M_construct_numeric,
      &locale::_Impl::_M_construct_collate,
      &locale::_Impl::_M_construct_time,
      &locale::_Impl::_M_construct_monetary,
      &locale::_Impl::_M_construct_messages,
      0
    };
    
    __vec_facet::iterator __it = _M_facets->begin();
    for (; __it != _M_facets->end(); ++__it)
      (*__it)->_M_add_reference();

    try 
      {
	unsigned mask = (locale::all & -(unsigned)locale::all);
	for (unsigned ix = 0; (-mask & __cat) != 0; ++ix, (mask <<= 1))
	  {
	    if (!(mask & __cat))
	      continue;
	    
	    if (mask & __cat)
	      _M_replace_category(_S_classic, _S_facet_categories[ix]);
	    else
	      (this->*ctors[ix])(__name.c_str());
	  }
      }
    catch(...) 
      {
	__it = _M_facets->begin();
	for (; __it != _M_facets->end(); ++__it)
	  (*__it)->_M_remove_reference();
	throw;
      }

    // XXX May need to be adjusted
    if (__cat == all)
      _M_name = __name;
  }
  
  void
  locale::_Impl::
  _M_replace_categories(const _Impl* __other, category __cat)
  {
    category  __mask = locale::all & -static_cast<unsigned int>(locale::all);
    for (unsigned int __ix = 0; (-__mask & __cat) != 0; ++__ix, (__mask <<= 1))
      {
	if (__mask & __cat)
	  {
	    _M_replace_category(__other, _S_facet_categories[__ix]);
	    (*_M_category_names)[__ix] = (*(__other->_M_category_names))[__ix];
	  }
      }
  }

  void
  locale::_Impl::
  _M_replace_category(const _Impl* __other, const locale::id* const* __idpp)
  {
    for (; *__idpp; ++__idpp)
      _M_replace_facet(__other, *__idpp);
  }
  
  void
  locale::_Impl::
  _M_replace_facet(const _Impl* __other, const locale::id* __idp)
  {
    size_t __index = __idp->_M_index;
    if (__index == 0 
	|| __other->_M_facets->size() <= __index 
	|| (*(__other->_M_facets))[__index] == 0)
      throw runtime_error("no locale facet");
	
    _M_install_facet(__idp, (*(__other->_M_facets))[__index]); 
  }

  void
  locale::_Impl::
  _M_install_facet(const locale::id* __idp, facet* __fp)
  {
    if (__fp == 0)
      return;

    size_t& __index = __idp->_M_index;
    if (!__index)
      __index = ++locale::id::_S_highwater;  // XXX MT

    if (__index >= _M_facets->size())
      _M_facets->resize(__index + 1, 0);  // might throw
    facet*& __fpr = (*_M_facets)[__index];
    // Order matters, here:
    __fp->_M_add_reference();
    if (__fpr) 
      __fpr->_M_remove_reference();
    __fpr = __fp;
  }
 
  void 
  locale::_Impl::_M_construct_collate(const char* __name)
  {
    _M_facet_init(new collate_byname<char>(__name, 0));
#ifdef _GLIBCPP_USE_WCHAR_T
    _M_facet_init(new collate_byname<wchar_t>(__name, 0));
#endif 
  }

  void 
  locale::_Impl::_M_construct_ctype(const char* __name)
  {
    _M_facet_init(new ctype_byname<char>(__name, 0));
    _M_facet_init(new codecvt_byname<char, char, mbstate_t>(__name));
#ifdef _GLIBCPP_USE_WCHAR_T
    _M_facet_init(new ctype_byname<wchar_t>(__name, 0));
    _M_facet_init(new codecvt_byname<wchar_t, char, mbstate_t>(__name));
#endif 
  }
    
  void 
  locale::_Impl::_M_construct_monetary(const char* __name)
  {
    _M_replace_facet(locale::_S_classic, &money_get<char>::id);
    _M_replace_facet(locale::_S_classic, &money_put<char>::id);
    _M_facet_init(new moneypunct_byname<char, false>(__name, 0));
    _M_facet_init(new moneypunct_byname<char, true >(__name, 0));
#ifdef _GLIBCPP_USE_WCHAR_T
    _M_replace_facet(locale::_S_classic, &money_get<wchar_t>::id);
    _M_replace_facet(locale::_S_classic, &money_put<wchar_t>::id);
    _M_facet_init(new moneypunct_byname<wchar_t, false>(__name, 0));
    _M_facet_init(new moneypunct_byname<wchar_t, true >(__name, 0));
#endif
  }
    
  void 
  locale::_Impl::_M_construct_numeric(const char* __name)
  {
    _M_replace_facet(locale::_S_classic, &num_get<char>::id);
    _M_replace_facet(locale::_S_classic, &num_put<char>::id);
    _M_facet_init(new numpunct_byname<char>(__name, 0));
#ifdef _GLIBCPP_USE_WCHAR_T
    _M_replace_facet(locale::_S_classic, &num_get<wchar_t>::id);
    _M_replace_facet(locale::_S_classic, &num_put<wchar_t>::id);
    _M_facet_init(new numpunct_byname<wchar_t>(__name, 0));
#endif 
  }
    
  void 
  locale::_Impl::_M_construct_time(const char* __name)
  {
    _M_facet_init(new time_get_byname<char>(__name, 0));
    _M_facet_init(new time_put_byname<char>(__name, 0));
#ifdef _GLIBCPP_USE_WCHAR_T
    _M_facet_init(new time_get_byname<wchar_t>(__name, 0));
    _M_facet_init(new time_put_byname<wchar_t>(__name, 0));
#endif 
  }
    
  void 
  locale::_Impl::_M_construct_messages(const char* __name)
  {
    _M_facet_init(new messages_byname<char>(__name, 0));
#ifdef _GLIBCPP_USE_WCHAR_T
    _M_facet_init(new messages_byname<wchar_t>(__name, 0));
#endif 
  }
}



