// Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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

namespace std
{
  locale::_Impl::
  ~_Impl() throw()
  {
    __vec_facet::iterator it = _M_facets->begin();
    for (; it != _M_facets->end(); ++it)
      if (*it)
	(*it)->_M_remove_reference();
    delete _M_facets;
  }

  // Clone existing _Impl object.
  locale::_Impl::
  _Impl(const _Impl& __imp, size_t __refs)
  : _M_references(__refs), _M_facets(0) // XXX
  {
    try
      {  _M_facets = new __vec_facet(*(__imp._M_facets)); }
    catch(...) 
      {
	delete _M_facets;
	__throw_exception_again;
      }

    for (size_t i = 0; i < _S_num_categories; ++i)
      _M_names[i] = __imp._M_names[i];

    __vec_facet::iterator __it = _M_facets->begin();
    for (; __it != _M_facets->end(); ++__it)
      if (*__it)
	(*__it)->_M_add_reference();
  }

  // Construct named _Impl, including the standard "C" locale.
  locale::_Impl::
  _Impl(string __str, size_t __refs)
  : _M_references(__refs), _M_facets(0)
  {
    // Initialize the underlying locale model, which also checks to
    // see if the given name is valid.
    __c_locale __cloc = NULL;
    if (__str != "C" && __str != "POSIX")
      locale::facet::_S_create_c_locale(__cloc, __str.c_str());

    // Allocate facet container.
    try
      {  _M_facets = new __vec_facet(_S_num_facets, NULL); }
    catch(...) 
      {
	delete _M_facets;
	__throw_exception_again;
      }

    // Name all the categories.
    for (size_t i = 0; i < _S_num_categories; ++i)
      _M_names[i] = __str;

    // Construct all standard facets and add them to _M_facets.
    // XXX Eventually, all should use __clocale ctor like numpunct
    // XXX how to deal cleanly, consistently with null ("C") __cloc?
    _M_init_facet(new std::collate<char>(__cloc));
    _M_init_facet(new std::ctype<char>);
    _M_init_facet(new codecvt<char, char, mbstate_t>);
    _M_init_facet(new moneypunct<char, false>(__cloc));
    _M_init_facet(new moneypunct<char,true>(__cloc));
    _M_init_facet(new money_get<char>);
    _M_init_facet(new money_put<char>);
    _M_init_facet(new numpunct<char>(__cloc));
    _M_init_facet(new num_get<char>);
    _M_init_facet(new num_put<char>);
    _M_init_facet(new time_get<char>);
    _M_init_facet(new time_put<char>);
    _M_init_facet(new std::messages<char>(__cloc, __str.c_str()));
    
#ifdef  _GLIBCPP_USE_WCHAR_T
    _M_init_facet(new std::collate<wchar_t>(__cloc));
    _M_init_facet(new std::ctype<wchar_t>);
    _M_init_facet(new codecvt<wchar_t, char, mbstate_t>);
    _M_init_facet(new moneypunct<wchar_t, false>(__cloc));
    _M_init_facet(new moneypunct<wchar_t,true>(__cloc));
    _M_init_facet(new money_get<wchar_t>);
    _M_init_facet(new money_put<wchar_t>);
    _M_init_facet(new numpunct<wchar_t>(__cloc));
    _M_init_facet(new num_get<wchar_t>);
    _M_init_facet(new num_put<wchar_t>);
    _M_init_facet(new time_get<wchar_t>);
    _M_init_facet(new time_put<wchar_t>);
    _M_init_facet(new std::messages<wchar_t>(__cloc, __str.c_str()));
#endif	  
    locale::facet::_S_destroy_c_locale(__cloc);
  }
  
  void
  locale::_Impl::
  _M_replace_categories(const _Impl* __imp, category __cat)
  {
    const string __none("*");
    category __mask;
    for (unsigned int __ix = 0; __ix < _S_num_categories; ++__ix)
      {
	__mask = 1 << __ix;
	if (__mask & __cat)
	  {
	    // Need to replace entry in _M_facets with other locale's info.
	    _M_replace_category(__imp, _S_facet_categories[__ix]);
	    // If both have names, go ahead and mangle.
	    if (_M_names[__ix] != __none && __imp->_M_names[__ix] != __none)
	      _M_names[__ix] = __imp->_M_names[__ix];
	  }
      }
  }

  void
  locale::_Impl::
  _M_replace_category(const _Impl* __imp, const locale::id* const* __idpp)
  {
    for (; *__idpp; ++__idpp)
      _M_replace_facet(__imp, *__idpp);
  }
  
  void
  locale::_Impl::
  _M_replace_facet(const _Impl* __imp, const locale::id* __idp)
  {
    size_t __index = __idp->_M_index;
    if (__index == 0 
	|| __imp->_M_facets->size() <= __index 
	|| (*(__imp->_M_facets))[__index] == 0)
      __throw_runtime_error("no locale facet");
	
    _M_install_facet(__idp, (*(__imp->_M_facets))[__index]); 
  }

  void
  locale::_Impl::
  _M_install_facet(const locale::id* __idp, facet* __fp)
  {
    if (__fp)
      {
	size_t& __index = __idp->_M_index;
	if (!__index)
	  __index = ++locale::id::_S_highwater;  // XXX MT
	
	if (__index >= _M_facets->size())
	  _M_facets->resize(__index + 1, 0);  // might throw

	facet*& __fpr = (*_M_facets)[__index];
	if (__fpr)
	  {
	    // Replacing an existing facet.
	    // Order matters, here:
	    __fp->_M_add_reference();
	    __fpr->_M_remove_reference();
	    __fpr = __fp;
	  }
	else
	  {
	    // Installing a newly created facet into an empty
	    // _M_facets container, say a newly-constructed,
	    // swanky-fresh _Impl.
	    (*_M_facets)[__index] = __fp;
	  }
      }
  }
} // namespace std
