
// Copyright (C) 1997-1999 Free Software Foundation, Inc.
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
#include <bits/std_cassert.h>
#include <bits/std_vector.h>

namespace std {

/////////////////////////////
// locale::_Impl constructors
/////////////////////////////

  // construct specific categories, leaving unselected ones alone
  //////////
  locale::_Impl::_Impl(const _Impl& other,const string& name, category cats,
		       size_t refs)
  : _M_num_references(refs)
    //  , _M_facets(other._M_facets)
    //  , _M_category_names(other._M_category_names)
    , _M_has_name(other._M_has_name), _M_cached_name_ok(false)
  {
#if 1
    typedef vector<facet*, allocator<facet*> > __vec_facet;
    typedef vector<string, allocator<string> > __vec_string;
    try {
      _M_facets = new __vec_facet(*(other._M_facets));
    }
    catch (...) {
      delete _M_facets;
      throw;
    }
    try {
       _M_category_names = new __vec_string(*(other._M_category_names));
    }
    catch (...) {
      delete _M_category_names;
      throw;
    }
#endif
    // XXX Nathan what are you doing here? Is this supposed to be const?
    // static void(_Impl::* const ctors[]) (const char*) = 
    static void(_Impl::* ctors[]) (const char*) = 
    {
      //  NB: order must match the decl order in class locale.
      &locale::_Impl::_M_construct_collate,
      &locale::_Impl::_M_construct_ctype,
      &locale::_Impl::_M_construct_monetary,
      &locale::_Impl::_M_construct_numeric,
      &locale::_Impl::_M_construct_time,
      &locale::_Impl::_M_construct_messages,
      0
    };
    
    _S_initialize();
    std::vector<facet*>::iterator it = _M_facets->begin();
    for (; it != _M_facets->end(); ++it)
      (*it)->_M_add_reference();

    try {
      category classix = _S_normalize_category(cats);  // might throw
      _M_normalize_category_names(name, classix);
	
      unsigned mask = (locale::all & -(unsigned)locale::all);
      for (unsigned ix = 0; (-mask & cats) != 0; ++ix, (mask <<= 1))
	{
	  if (!(mask & cats))
	    continue;
	  
	  if (mask & classix)
	    _M_replace_category(_S_classic, _S_facet_categories[ix]);
	  else
	    (this->*ctors[ix]) (name.c_str());
	}
    }
    catch (...) {
      it = _M_facets->begin();
      for (; it != _M_facets->end(); ++it)
	(*it)->_M_remove_reference();
      throw;
    }
  }

  //////////
  locale::category
  locale::_Impl::_M_normalize_category_names(const string&, 
					     locale::category cats)
  {

    // The problem to be solved here is that locale names
    //   generally have one of two forms: they might have
    //   only one component, such as "en_US"; or they might
    //   have six, such as "en_US fr_FR en_US C C C", where
    //   each component names a category.  Each vendor has
    //   a different order of categories.  Each vendor uses
    //   a different format:
    //      AIX uses "C C C C C C"
    //      Sun uses "/C/C/C/C/C/C"
    //      HP uses  "/0:C;1:C;2:C;3:C;4:C;5:C;6:C;/"
    //        (where the 0th element is for LC_ALL.)
    //   Most systems (except AIX) permit the long form only for
    //   setlocale(LC_ALL,...), and require the short form for
    //   other calls.  All this matters because locale names are
    //   supposed to be compatible between locale("") and
    //   setlocale(..., "") constructors.
    
    return cats;
#if 0 /* XXX not done */
    unsigned mask = (locale::all & -(unsigned)locale::all);
    for (unsigned ix = 0; (-mask & cats) != 0; ++ix, (mask <<= 1))
      {
	
      }
#endif
  }

  //////////
  void 
  locale::_Impl::_M_construct_collate(const char* /*name*/)
  {
#if 0
    _M_init_facet(new std::collate_byname<char>(name));
    _M_init_facet(new std::collate_byname<wchar_t>(name));
#endif
  }

  void 
  locale::_Impl::_M_construct_ctype(const char* /*name*/)
  {
#if 0
    _M_init_facet(new std::ctype_byname<char>(name));
    _M_init_facet(new std::ctype_byname<wchar_t>(name));
    _M_init_facet(new std::codecvt_byname<char,char,mbstate_t>(name));
    _M_init_facet(new std::codecvt_byname<wchar_t,char,mbstate_t>(name));
#endif
  }
    
  void 
  locale::_Impl::_M_construct_monetary(const char* /*name*/)
  {
#if 0
    _M_init_facet(new std::moneypunct_byname<char,false>(name));
    _M_init_facet(new std::moneypunct_byname<wchar_t,false>(name));
    _M_init_facet(new std::moneypunct_byname<char,true >(name));
    _M_init_facet(new std::moneypunct_byname<wchar_t,true >(name));

    locale::_M_initialize();
    _M_replace_facet(locale::_S_classic, &std::money_get<char>(name)::id);
    _M_replace_facet(locale::_S_classic, &std::money_get<wchar_t>(name)::id);
    _M_replace_facet(locale::_S_classic, &std::money_put<char>(name)::id);
    _M_replace_facet(locale::_S_classic, &std::money_put<wchar_t>(name)::id);
#endif
  }
    
  void 
  locale::_Impl::_M_construct_numeric(const char* /*name*/)
  {
#if 0
    _M_init_facet(new std::numpunct_byname<char>(name));
    _M_init_facet(new std::numpunct_byname<wchar_t>(name));

    locale::_M_initialize();
    _M_replace_facet(locale::_S_classic, &std::num_get<char>::id);
    _M_replace_facet(locale::_S_classic, &std::num_get<wchar_t>::id);
    _M_replace_facet(locale::_S_classic, &std::num_put<char>::id);
    _M_replace_facet(locale::_S_classic, &std::num_put<wchar_t>::id);
#endif
  }
    
  void 
  locale::_Impl::_M_construct_time(const char* /*name*/)
  {
#if 0
    _M_init_facet(new std::time_get_byname<char>(name));
    _M_init_facet(new std::time_get_byname<wchar_t>(name));
    _M_init_facet(new std::time_put_byname<char>(name));
    _M_init_facet(new std::time_put_byname<wchar_t>(name));
#endif
  }
    
  void 
  locale::_Impl::_M_construct_messages(const char* /*name*/)
  {
#if 0
    _M_init_facet(new std::messages_byname<char>(name));
    _M_init_facet(new std::messages_byname<wchar_t>(name));
#endif
  }

  //////////////////////
  // locale constructors
  //////////////////////
  
  ////////
  locale::locale(const char* std_name)
  {
    _S_initialize();
    if (strcmp(std_name, "C") == 0 || strcmp(std_name, "POSIX"))
      (_M_impl = _S_classic)->_M_add_reference();
    else
      {
	// might throw:
	_M_impl = new _Impl(*_S_classic, string(std_name), all, 1);
        _M_impl->_M_has_name = true;
      }
  }

  /////////
  locale::locale(const locale& other, const char* std_name, category cats)
  : _M_impl(new _Impl(*other._M_impl, string(std_name),
		      _S_normalize_category(cats), 1))  // might throw
  { }

  ///////
  bool
  locale::operator==(const locale& __rhs) const throw()
  {
    return(_M_impl == __rhs._M_impl 
	   || (this->name() != "*" && this->name() == __rhs.name()));
  }

}
