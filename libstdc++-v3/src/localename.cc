// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
// Free Software Foundation, Inc.
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

#include <clocale>
#include <cstring>
#include <locale>

namespace std
{
  // Defined in globals.cc.
  extern std::ctype<char>			ctype_c;
  extern std::collate<char> 			collate_c;
  extern numpunct<char> 			numpunct_c;
  extern num_get<char> 				num_get_c;
  extern num_put<char> 				num_put_c;
  extern codecvt<char, char, mbstate_t>		codecvt_c;
  extern moneypunct<char, false> 		moneypunct_fc;
  extern moneypunct<char, true> 		moneypunct_tc;
  extern money_get<char> 			money_get_c;
  extern money_put<char> 			money_put_c;
  extern __timepunct<char> 			timepunct_c;
  extern time_get<char> 			time_get_c;
  extern time_put<char> 			time_put_c;
  extern std::messages<char> 			messages_c;
#ifdef  _GLIBCPP_USE_WCHAR_T
  extern std::ctype<wchar_t>			ctype_w;
  extern std::collate<wchar_t> 			collate_w;
  extern numpunct<wchar_t> 			numpunct_w;
  extern num_get<wchar_t> 			num_get_w;
  extern num_put<wchar_t> 			num_put_w;
  extern codecvt<wchar_t, char, mbstate_t>	codecvt_w;
  extern moneypunct<wchar_t, false> 		moneypunct_fw;
  extern moneypunct<wchar_t, true> 		moneypunct_tw;
  extern money_get<wchar_t> 			money_get_w;
  extern money_put<wchar_t> 			money_put_w;
  extern __timepunct<wchar_t> 			timepunct_w;
  extern time_get<wchar_t> 			time_get_w;
  extern time_put<wchar_t> 			time_put_w;
  extern std::messages<wchar_t> 		messages_w;
#endif

  locale::_Impl::
  ~_Impl() throw()
  {
    for (size_t __i = 0; __i < _M_facets_size; ++__i)
      if (_M_facets[__i])
	_M_facets[__i]->_M_remove_reference();
    delete [] _M_facets;
  }

  // Clone existing _Impl object.
  locale::_Impl::
  _Impl(const _Impl& __imp, size_t __refs)
  : _M_references(__refs), _M_facets_size(__imp._M_facets_size) // XXX
  {
    try
      { 
	_M_facets = new facet*[_M_facets_size]; 
	for (size_t __i = 0; __i < _M_facets_size; ++__i)
	  _M_facets[__i] = 0;
      }
    catch(...) 
      {
	delete [] _M_facets;
	__throw_exception_again;
      }
    for (size_t __i = 0; __i < _M_facets_size; ++__i)
      {
	_M_facets[__i] = __imp._M_facets[__i];
	if (_M_facets[__i])
	  _M_facets[__i]->_M_add_reference();
      }
    for (size_t __i = 0; __i < _S_num_categories; ++__i)
      _M_names[__i] = __imp._M_names[__i];
  }

  // Construct named _Impl.
  locale::_Impl::
  _Impl(const char* __s, size_t __refs) 
  : _M_references(__refs), _M_facets_size(_GLIBCPP_NUM_FACETS) // XXX
  {
    // Initialize the underlying locale model, which also checks
    // to see if the given name is valid.
    __c_locale __cloc;
    locale::facet::_S_create_c_locale(__cloc, __s);

    try
      { 
	_M_facets = new facet*[_M_facets_size]; 
	for (size_t __i = 0; __i < _M_facets_size; ++__i)
	  _M_facets[__i] = 0;
      }
    catch(...) 
      {
	delete [] _M_facets;
	__throw_exception_again;
      }

    // Name all the categories.
    for (size_t i = 0; i < _S_num_categories; ++i)
      _M_names[i] = __s;

    // Construct all standard facets and add them to _M_facets.
    _M_init_facet(new std::ctype<char>(__cloc));
    _M_init_facet(new codecvt<char, char, mbstate_t>);
    _M_init_facet(new numpunct<char>(__cloc));
    _M_init_facet(new num_get<char>);
    _M_init_facet(new num_put<char>);
    _M_init_facet(new std::collate<char>(__cloc));
    _M_init_facet(new moneypunct<char, false>(__cloc, __s));
    _M_init_facet(new moneypunct<char, true>(__cloc, __s));
    _M_init_facet(new money_get<char>);
    _M_init_facet(new money_put<char>);
    _M_init_facet(new __timepunct<char>(__cloc, __s));
    _M_init_facet(new time_get<char>);
    _M_init_facet(new time_put<char>);
    _M_init_facet(new std::messages<char>(__cloc, __s));
	
#ifdef  _GLIBCPP_USE_WCHAR_T
    _M_init_facet(new std::ctype<wchar_t>(__cloc));
    _M_init_facet(new codecvt<wchar_t, char, mbstate_t>);
    _M_init_facet(new numpunct<wchar_t>(__cloc));
    _M_init_facet(new num_get<wchar_t>);
    _M_init_facet(new num_put<wchar_t>);
    _M_init_facet(new std::collate<wchar_t>(__cloc));
    _M_init_facet(new moneypunct<wchar_t, false>(__cloc, __s));
    _M_init_facet(new moneypunct<wchar_t, true>(__cloc, __s));
    _M_init_facet(new money_get<wchar_t>);
    _M_init_facet(new money_put<wchar_t>);
    _M_init_facet(new __timepunct<wchar_t>(__cloc, __s));
    _M_init_facet(new time_get<wchar_t>);
    _M_init_facet(new time_put<wchar_t>);
    _M_init_facet(new std::messages<wchar_t>(__cloc, __s));
#endif	  
    locale::facet::_S_destroy_c_locale(__cloc);
  }

  // Construct "C" _Impl.
  locale::_Impl::
  _Impl(facet** __f, size_t __refs, bool) 
  : _M_references(__refs), _M_facets(__f), _M_facets_size(_GLIBCPP_NUM_FACETS)
  {
    // Name all the categories.
    for (size_t i = 0; i < _S_num_categories; ++i)
      _M_names[i] = "C";

    // This is needed as presently the C++ version of "C" locales
    // != data in the underlying locale model for __timepunct,
    // numpunct, and moneypunct. Also, the "C" locales must be
    // constructed in a way such that they are pre-allocated.
    _M_init_facet(new (&ctype_c) std::ctype<char>);
    _M_init_facet(new (&codecvt_c) codecvt<char, char, mbstate_t>);
    _M_init_facet(new (&numpunct_c) numpunct<char>);
    _M_init_facet(new (&num_get_c) num_get<char>);
    _M_init_facet(new (&num_put_c) num_put<char>);
    _M_init_facet(new (&collate_c) std::collate<char>);
    _M_init_facet(new (&moneypunct_fc) moneypunct<char, false>);
    _M_init_facet(new (&moneypunct_tc) moneypunct<char, true>);
    _M_init_facet(new (&money_get_c) money_get<char>);
    _M_init_facet(new (&money_put_c) money_put<char>);
    _M_init_facet(new (&timepunct_c) __timepunct<char>);
    _M_init_facet(new (&time_get_c) time_get<char>);
    _M_init_facet(new (&time_put_c) time_put<char>);
    _M_init_facet(new (&messages_c) std::messages<char>);	
#ifdef  _GLIBCPP_USE_WCHAR_T
    _M_init_facet(new (&ctype_w) std::ctype<wchar_t>);
    _M_init_facet(new (&codecvt_w) codecvt<wchar_t, char, mbstate_t>);
    _M_init_facet(new (&numpunct_w) numpunct<wchar_t>);
    _M_init_facet(new (&num_get_w) num_get<wchar_t>);
    _M_init_facet(new (&num_put_w) num_put<wchar_t>);
    _M_init_facet(new (&collate_w) std::collate<wchar_t>);
    _M_init_facet(new (&moneypunct_fw) moneypunct<wchar_t, false>);
    _M_init_facet(new (&moneypunct_tw) moneypunct<wchar_t, true>);
    _M_init_facet(new (&money_get_w) money_get<wchar_t>);
    _M_init_facet(new (&money_put_w) money_put<wchar_t>);
    _M_init_facet(new (&timepunct_w) __timepunct<wchar_t>);
    _M_init_facet(new (&time_get_w) time_get<wchar_t>);
    _M_init_facet(new (&time_put_w) time_put<wchar_t>);
    _M_init_facet(new (&messages_w) std::messages<wchar_t>);
#endif	  
  }
  
  void
  locale::_Impl::
  _M_replace_categories(const _Impl* __imp, category __cat)
  {
    category __mask;
    for (unsigned int __ix = 0; __ix < _S_num_categories; ++__ix)
      {
	__mask = 1 << __ix;
	if (__mask & __cat)
	  {
	    // Need to replace entry in _M_facets with other locale's info.
	    _M_replace_category(__imp, _S_facet_categories[__ix]);
	    // If both have names, go ahead and mangle.
	    if (strcmp(_M_names[__ix], "*") != 0 
		&& strcmp(__imp->_M_names[__ix], "*") != 0)
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
    size_t __index = __idp->_M_id();
    if ((__index > (__imp->_M_facets_size - 1)) || !__imp->_M_facets[__index])
      __throw_runtime_error("no locale facet");
    _M_install_facet(__idp, __imp->_M_facets[__index]); 
  }

  void
  locale::_Impl::
  _M_install_facet(const locale::id* __idp, facet* __fp)
  {
    if (__fp)
      {
	size_t __index = __idp->_M_id();
	if (__index > _M_facets_size - 1)
	  {
	    facet** __old = _M_facets;
	    facet** __new;
	    const size_t __new_size = __index + 4;
	    __new = new facet*[__new_size]; 
	    for (size_t __i = 0; __i < _M_facets_size; ++__i)
	      __new[__i] = _M_facets[__i];
	    for (size_t __i2 = _M_facets_size; __i2 < __new_size; ++__i2)
	      __new[__i2] = 0;

	    _M_facets_size = __new_size;
	    _M_facets = __new;
	    delete [] __old;
	  }

	facet*& __fpr = _M_facets[__index];
	if (__fpr)
	  {
	    // Replacing an existing facet. Order matters.
	    __fp->_M_add_reference();
	    __fpr->_M_remove_reference();
	    __fpr = __fp;
	  }
	else
	  {
	    // Installing a newly created facet into an empty
	    // _M_facets container, say a newly-constructed,
	    // swanky-fresh _Impl.
	    _M_facets[__index] = __fp;
	  }
      }
  }
} // namespace std
