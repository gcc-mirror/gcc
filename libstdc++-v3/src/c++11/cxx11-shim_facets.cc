// Locale support -*- C++ -*-

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

//
// ISO C++ 14882: 22.1  Locales
//

// This file defines classes that behave like the standard predefined locale
// facets (collate, money_get etc.) except that they forward all virtual
// functions to another facet which uses a different std::string ABI,
// converting between string types as needed.
// When a user replaces one of the relevant facets the corresponding shim in
// this file is used so that the replacement facet can be used (via the shim)
// in code that uses the other std::string ABI from the replacing code.

#ifndef _GLIBCXX_USE_CXX11_ABI
# define _GLIBCXX_USE_CXX11_ABI 1
#endif
#include <locale>

#if ! _GLIBCXX_USE_DUAL_ABI
# error This file should not be compiled for this configuration.
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
  // Base class of facet shims, holds a reference to the underlying facet
  // that the shim forwards to.
  class locale::facet::__shim
  {
  public:
    const facet* _M_get() const { return _M_facet; }

    __shim(const __shim&) = delete;
    __shim& operator=(const __shim&) = delete;

  protected:
    explicit
    __shim(const facet* __f) : _M_facet(__f) { __f->_M_add_reference(); }

    ~__shim() { _M_facet->_M_remove_reference(); }

  private:
    const facet* _M_facet;
  };

namespace __facet_shims
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  namespace // unnamed
  {
    template<typename C>
      void __destroy_string(void* p)
      {
	static_cast<std::basic_string<C>*>(p)->~basic_string();
      }
  } // namespace

  // Manages a buffer of uninitialized memory that can store a std::string
  // or std::wstring, using either ABI, and convert to the other ABI.
  class __any_string
  {
    struct __attribute__((may_alias)) __str_rep
    {
      union {
	const void* _M_p;
	char* _M_pc;
#ifdef _GLIBCXX_USE_WCHAR_T
	wchar_t* _M_pwc;
#endif
      };
      size_t _M_len;
      char _M_unused[16];

      operator const char*() const { return _M_pc; }
#ifdef _GLIBCXX_USE_WCHAR_T
      operator const wchar_t*() const { return _M_pwc; }
#endif
    };
    union {
      __str_rep _M_str;
      char _M_bytes[sizeof(__str_rep)];
    };
    using __dtor_func = void(*)(void*);
    __dtor_func _M_dtor = nullptr;

#if _GLIBCXX_USE_CXX11_ABI
    // SSO strings overlay the entire __str_rep structure.
    static_assert(sizeof(std::string) == sizeof(__str_rep),
		  "std::string changed size!");
#else
    // COW strings overlay just the pointer, the length is stored manually.
    static_assert(sizeof(std::string) == sizeof(__str_rep::_M_p),
		  "std::string changed size!");
#endif
# ifdef _GLIBCXX_USE_WCHAR_T
    static_assert(sizeof(std::wstring) == sizeof(std::string),
		  "std::wstring and std::string are different sizes!");
# endif

  public:
    __any_string() = default;
    ~__any_string() { if (_M_dtor) _M_dtor(_M_bytes); }

    __any_string(const __any_string&) = delete;
    __any_string& operator=(const __any_string&) = delete;

    // Store a string (and its length if needed) in the buffer and
    // set _M_dtor to the function that runs the right destructor.
    template<typename C>
      __any_string&
      operator=(const basic_string<C>& s)
      {
	if (_M_dtor)
	  _M_dtor(_M_bytes);
	::new(_M_bytes) basic_string<C>(s);
#if ! _GLIBCXX_USE_CXX11_ABI
	_M_str._M_len = s.length();
#endif
	_M_dtor = __destroy_string<C>;
	return *this;
      }

    // Create a new string with a copy of the characters in the stored string.
    // The returned object will match the caller's string ABI, even when the
    // stored string doesn't.
    template<typename C>
      _GLIBCXX_DEFAULT_ABI_TAG
      operator basic_string<C>() const
      {
	if (!_M_dtor)
	  __throw_logic_error("uninitialized __any_string");
	return basic_string<C>(static_cast<const C*>(_M_str), _M_str._M_len);
      }
  };

  // This file is compiled twice, with and without this macro defined.
  // Define tag types to distinguish between the two cases and to allow
  // overloading on the tag.
  using current_abi = __bool_constant<_GLIBCXX_USE_CXX11_ABI>;
  using other_abi = __bool_constant<!_GLIBCXX_USE_CXX11_ABI>;

  using facet = locale::facet;

  // Declare the functions that shims defined in this file will call to
  // perform work in the context of the other ABI.
  // These will be defined when this file is recompiled for the other ABI
  // (at which point what is now "current_abi" will become "other_abi").

  template<typename C>
    void
    __numpunct_fill_cache(other_abi, const facet*, __numpunct_cache<C>*);

  template<typename C>
    int
    __collate_compare(other_abi, const facet*, const C*, const C*,
		      const C*, const C*);

  template<typename C>
    void
    __collate_transform(other_abi, const facet*, __any_string&,
			const C*, const C*);

  template<typename C>
    time_base::dateorder
    __time_get_dateorder(other_abi, const facet* f);

  template<typename C>
    istreambuf_iterator<C>
    __time_get(other_abi, const facet* f,
	       istreambuf_iterator<C> beg, istreambuf_iterator<C> end,
	       ios_base& io, ios_base::iostate& err, tm* t, char which);

  template<typename C, bool Intl>
    void
    __moneypunct_fill_cache(other_abi, const facet*,
			    __moneypunct_cache<C, Intl>*);

  template<typename C>
    istreambuf_iterator<C>
    __money_get(other_abi, const facet*,
		istreambuf_iterator<C>, istreambuf_iterator<C>,
		bool, ios_base&, ios_base::iostate&,
		long double*, __any_string*);

  template<typename C>
    ostreambuf_iterator<C>
    __money_put(other_abi, const facet*, ostreambuf_iterator<C>, bool,
		ios_base&, C, long double, const __any_string*);

  template<typename C>
    messages_base::catalog
    __messages_open(other_abi, const facet*, const char*, size_t,
		    const locale&);

  template<typename C>
    void
    __messages_get(other_abi, const facet*, __any_string&,
		   messages_base::catalog, int, int, const C*, size_t);

  template<typename C>
    void
    __messages_close(other_abi, const facet*, messages_base::catalog);

  namespace // unnamed
  {
    struct __shim_accessor : facet
    {
      using facet::__shim;  // Redeclare protected member as public.
    };
    using __shim = __shim_accessor::__shim;

    template<typename _CharT>
      struct numpunct_shim : std::numpunct<_CharT>, __shim
      {
	typedef typename numpunct<_CharT>::__cache_type __cache_type;

	// f must point to a type derived from numpunct<C>[abi:other]
	numpunct_shim(const facet* f, __cache_type* c = new __cache_type)
	: std::numpunct<_CharT>(c), __shim(f), _M_cache(c)
	{
	  __numpunct_fill_cache(other_abi{}, f, c);
	}

	~numpunct_shim()
	{
	  // Stop GNU locale's ~numpunct() from freeing the cached string.
	  _M_cache->_M_grouping_size = 0;
	}

	// No need to override any virtual functions, the base definitions
	// will return the cached data.

	__cache_type* _M_cache;
      };

    template<typename _CharT>
      struct collate_shim : std::collate<_CharT>, __shim
      {
	typedef basic_string<_CharT>	string_type;

	// f must point to a type derived from collate<C>[abi:other]
	collate_shim(const facet* f) : __shim(f) { }

	virtual int
	do_compare(const _CharT* lo1, const _CharT* hi1,
		   const _CharT* lo2, const _CharT* hi2) const
	{
	  return __collate_compare(other_abi{}, _M_get(),
				   lo1, hi1, lo2, hi2);
	}

	virtual string_type
	do_transform(const _CharT* lo, const _CharT* hi) const
	{
	  __any_string st;
	  __collate_transform(other_abi{}, _M_get(), st, lo, hi);
	  return st;
	}
      };

    template<typename _CharT>
      struct time_get_shim : std::time_get<_CharT>, __shim
      {
	typedef typename std::time_get<_CharT>::iter_type iter_type;
	typedef typename std::time_get<_CharT>::char_type char_type;

	// f must point to a type derived from time_get<C>[abi:other]
	time_get_shim(const facet* f) : __shim(f) { }

	virtual time_base::dateorder
	do_date_order() const
	{ return __time_get_dateorder<_CharT>(other_abi{}, _M_get()); }

	virtual iter_type
	do_get_time(iter_type beg, iter_type end, ios_base& io,
		    ios_base::iostate& err, tm* t) const
	{
	  return __time_get(other_abi{}, _M_get(), beg, end, io, err, t,
			    't');
	}

	virtual iter_type
	do_get_date(iter_type beg, iter_type end, ios_base& io,
		    ios_base::iostate& err, tm* t) const
	{
	  return __time_get(other_abi{}, _M_get(), beg, end, io, err, t,
			    'd');
	}

	virtual iter_type
	do_get_weekday(iter_type beg, iter_type end, ios_base& io,
		       ios_base::iostate& err, tm* t) const
	{
	  return __time_get(other_abi{}, _M_get(), beg, end, io, err, t,
			    'w');
	}

	virtual iter_type
	do_get_monthname(iter_type beg, iter_type end, ios_base& io,
			 ios_base::iostate& err, tm* t) const
	{
	  return __time_get(other_abi{}, _M_get(), beg, end, io, err, t,
			    'm');
	}

	virtual iter_type
	do_get_year(iter_type beg, iter_type end, ios_base& io,
		    ios_base::iostate& err, tm* t) const
	{
	  return __time_get(other_abi{}, _M_get(), beg, end, io, err, t,
			    'y');
	}
      };

    template<typename _CharT, bool _Intl>
      struct moneypunct_shim : std::moneypunct<_CharT, _Intl>, __shim
      {
	typedef typename moneypunct<_CharT, _Intl>::__cache_type __cache_type;

	// f must point to a type derived from moneypunct<C>[abi:other]
	moneypunct_shim(const facet* f, __cache_type* c = new __cache_type)
	: std::moneypunct<_CharT, _Intl>(c), __shim(f), _M_cache(c)
	{
	  __moneypunct_fill_cache(other_abi{}, f, c);
	}

	~moneypunct_shim()
	{
	  // Stop GNU locale's ~moneypunct() from freeing the cached strings.
	  _M_cache->_M_grouping_size = 0;
	  _M_cache->_M_curr_symbol_size = 0;
	  _M_cache->_M_positive_sign_size = 0;
	  _M_cache->_M_negative_sign_size = 0;
	}

	// No need to override any virtual functions, the base definitions
	// will return the cached data.

	__cache_type* _M_cache;
      };

    template<typename _CharT>
      struct money_get_shim : std::money_get<_CharT>, __shim
      {
	typedef typename std::money_get<_CharT>::iter_type iter_type;
	typedef typename std::money_get<_CharT>::char_type char_type;
	typedef typename std::money_get<_CharT>::string_type string_type;

	// f must point to a type derived from money_get<C>[abi:other]
	money_get_shim(const facet* f) : __shim(f) { }

	virtual iter_type
	do_get(iter_type s, iter_type end, bool intl, ios_base& io,
	       ios_base::iostate& err, long double& units) const
	{
	  ios_base::iostate err2 = ios_base::goodbit;
	  long double units2;
	  s = __money_get(other_abi{}, _M_get(), s, end, intl, io, err2,
			  &units2, nullptr);
	  if (err2 == ios_base::goodbit)
	    units = units2;
	  else
	    err = err2;
	  return s;
	}

	virtual iter_type
	do_get(iter_type s, iter_type end, bool intl, ios_base& io,
	       ios_base::iostate& err, string_type& digits) const
	{
	  __any_string st;
	  ios_base::iostate err2 = ios_base::goodbit;
	  s = __money_get(other_abi{}, _M_get(), s, end, intl, io, err2,
			  nullptr, &st);
	  if (err2 == ios_base::goodbit)
	    digits = st;
	  else
	    err = err2;
	  return s;
	}
      };

    template<typename _CharT>
      struct money_put_shim : std::money_put<_CharT>, __shim
      {
	typedef typename std::money_put<_CharT>::iter_type iter_type;
	typedef typename std::money_put<_CharT>::char_type char_type;
	typedef typename std::money_put<_CharT>::string_type string_type;

	// f must point to a type derived from money_put<C>[abi:other]
	money_put_shim(const facet* f) : __shim(f) { }

	virtual iter_type
	do_put(iter_type s, bool intl, ios_base& io,
	       char_type fill, long double units) const
	{
	  return __money_put(other_abi{}, _M_get(), s, intl, io, fill, units,
			     nullptr);
	}

	virtual iter_type
	do_put(iter_type s, bool intl, ios_base& io,
	       char_type fill, const string_type& digits) const
	{
	  __any_string st;
	  st = digits;
	  return __money_put(other_abi{}, _M_get(), s, intl, io, fill, 0.L,
			     &st);
	}
      };

    template<typename _CharT>
      struct messages_shim : std::messages<_CharT>, __shim
      {
	typedef messages_base::catalog  catalog;
	typedef basic_string<_CharT>	string_type;

	// f must point to a type derived from messages<C>[abi:other]
	messages_shim(const facet* f) : __shim(f) { }

	virtual catalog
	do_open(const basic_string<char>& s, const locale& l) const
	{
	  return __messages_open<_CharT>(other_abi{}, _M_get(),
					 s.c_str(), s.size(), l);
	}

	virtual string_type
	do_get(catalog c, int set, int msgid, const string_type& dfault) const
	{
	  __any_string st;
	  __messages_get(other_abi{}, _M_get(), st, c, set, msgid,
			 dfault.c_str(), dfault.size());
	  return st;
	}

	virtual void
	do_close(catalog c) const
	{
	  __messages_close<_CharT>(other_abi{}, _M_get(), c);
	}
      };

    template class numpunct_shim<char>;
    template class collate_shim<char>;
    template class moneypunct_shim<char, true>;
    template class moneypunct_shim<char, false>;
    template class money_get_shim<char>;
    template class money_put_shim<char>;
    template class messages_shim<char>;
#ifdef _GLIBCXX_USE_WCHAR_T
    template class numpunct_shim<wchar_t>;
    template class collate_shim<wchar_t>;
    template class moneypunct_shim<wchar_t, true>;
    template class moneypunct_shim<wchar_t, false>;
    template class money_get_shim<wchar_t>;
    template class money_put_shim<wchar_t>;
    template class messages_shim<wchar_t>;
#endif

    template<typename C>
      inline size_t
      __copy(const C*& dest, const basic_string<C>& s)
      {
	auto len = s.length();
	C* p = new C[len+1];
	s.copy(p, len);
	p[len] = '\0';
	dest = p;
	return len;
      }

  } // namespace

  // Now define and instantiate the functions that will be called by the
  // shim facets defined when this file is recompiled for the other ABI.

  // Cache the values returned by the numpunct facet f.
  // Sets c->_M_allocated so that the __numpunct_cache destructor will
  // delete[] the strings allocated by this function.
  template<typename C>
    void
    __numpunct_fill_cache(current_abi, const facet* f, __numpunct_cache<C>* c)
    {
      auto* m = static_cast<const numpunct<C>*>(f);

      c->_M_decimal_point = m->decimal_point();
      c->_M_thousands_sep = m->thousands_sep();

      c->_M_grouping = nullptr;
      c->_M_truename = nullptr;
      c->_M_falsename = nullptr;
      // set _M_allocated so that if any allocation fails the previously
      // allocated strings will be deleted in ~__numpunct_cache()
      c->_M_allocated = true;

      c->_M_grouping_size = __copy(c->_M_grouping, m->grouping());
      c->_M_truename_size = __copy(c->_M_truename, m->truename());
      c->_M_falsename_size = __copy(c->_M_falsename, m->falsename());
    }

  template void
  __numpunct_fill_cache(current_abi, const facet*, __numpunct_cache<char>*);

#ifdef _GLIBCXX_USE_WCHAR_T
  template void
  __numpunct_fill_cache(current_abi, const facet*, __numpunct_cache<wchar_t>*);
#endif

  template<typename C>
    int
    __collate_compare(current_abi, const facet* f, const C* lo1, const C* hi1,
		      const C* lo2, const C* hi2)
    {
      return static_cast<const collate<C>*>(f)->compare(lo1, hi1, lo2, hi2);
    }

  template int
  __collate_compare(current_abi, const facet*, const char*, const char*,
		    const char*, const char*);

#ifdef _GLIBCXX_USE_WCHAR_T
  template int
  __collate_compare(current_abi, const facet*, const wchar_t*, const wchar_t*,
		    const wchar_t*, const wchar_t*);
#endif

  template<typename C>
    void
    __collate_transform(current_abi, const facet* f, __any_string& st,
			const C* __lo, const C* __hi)
    {
      auto* c = static_cast<const collate<C>*>(f);
      st = c->transform(__lo, __hi);
    }

  template void
  __collate_transform(current_abi, const facet*, __any_string&,
		      const char*, const char*);

#ifdef _GLIBCXX_USE_WCHAR_T
  template void
  __collate_transform(current_abi, const facet*, __any_string&,
		      const wchar_t*, const wchar_t*);
#endif

  // Cache the values returned by the moneypunct facet, f.
  // Sets c->_M_allocated so that the __moneypunct_cache destructor will
  // delete[] the strings allocated by this function.
  template<typename C, bool Intl>
    void
    __moneypunct_fill_cache(current_abi, const facet* f,
			    __moneypunct_cache<C, Intl>* c)
    {
      auto* m = static_cast<const moneypunct<C, Intl>*>(f);

      c->_M_decimal_point = m->decimal_point();
      c->_M_thousands_sep = m->thousands_sep();
      c->_M_frac_digits = m->frac_digits();

      c->_M_grouping = nullptr;
      c->_M_curr_symbol = nullptr;
      c->_M_positive_sign = nullptr;
      c->_M_negative_sign = nullptr;
      // Set _M_allocated so that if any allocation fails the previously
      // allocated strings will be deleted in ~__moneypunct_cache().
      c->_M_allocated = true;

      c->_M_grouping_size = __copy(c->_M_grouping, m->grouping());
      c->_M_curr_symbol_size = __copy(c->_M_curr_symbol, m->curr_symbol());
      c->_M_positive_sign_size
	= __copy(c->_M_positive_sign, m->positive_sign());
      c->_M_negative_sign_size
	= __copy(c->_M_negative_sign, m->negative_sign());

      c->_M_pos_format = m->pos_format();
      c->_M_neg_format = m->neg_format();
    }

  template void
  __moneypunct_fill_cache(current_abi, const facet*,
			  __moneypunct_cache<char, true>*);

  template void
  __moneypunct_fill_cache(current_abi, const facet*,
			  __moneypunct_cache<char, false>*);

#ifdef _GLIBCXX_USE_WCHAR_T
  template void
  __moneypunct_fill_cache(current_abi, const facet*,
			  __moneypunct_cache<wchar_t, true>*);

  template void
  __moneypunct_fill_cache(current_abi, const facet*,
			  __moneypunct_cache<wchar_t, false>*);
#endif

  template<typename C>
    messages_base::catalog
    __messages_open(current_abi, const facet* f, const char* s, size_t n,
		    const locale& l)
    {
      auto* m = static_cast<const messages<C>*>(f);
      string str(s, n);
      return m->open(str, l);
    }

  template messages_base::catalog
  __messages_open<char>(current_abi, const facet*, const char*, size_t,
			const locale&);

#ifdef _GLIBCXX_USE_WCHAR_T
  template messages_base::catalog
  __messages_open<wchar_t>(current_abi, const facet*, const char*, size_t,
			   const locale&);
#endif

  template<typename C>
    void
    __messages_get(current_abi, const facet* f, __any_string& st,
		   messages_base::catalog c, int set, int msgid,
		   const C* s, size_t n)
    {
      auto* m = static_cast<const messages<C>*>(f);
      st = m->get(c, set, msgid, basic_string<C>(s, n));
    }

  template void
  __messages_get(current_abi, const facet*, __any_string&,
		 messages_base::catalog, int, int, const char*, size_t);

#ifdef _GLIBCXX_USE_WCHAR_T
  template void
  __messages_get(current_abi, const facet*, __any_string&,
		 messages_base::catalog, int, int, const wchar_t*, size_t);
#endif

  template<typename C>
    void
    __messages_close(current_abi, const facet* f, messages_base::catalog c)
    {
      static_cast<const messages<C>*>(f)->close(c);
    }

  template void
  __messages_close<char>(current_abi, const facet*, messages_base::catalog c);

#ifdef _GLIBCXX_USE_WCHAR_T
  template void
  __messages_close<wchar_t>(current_abi, const facet*,
			    messages_base::catalog c);
#endif

  template<typename C>
    time_base::dateorder
    __time_get_dateorder(current_abi, const facet* f)
    { return static_cast<const time_get<C>*>(f)->date_order(); }

  template time_base::dateorder
  __time_get_dateorder<char>(current_abi, const facet*);

#ifdef _GLIBCXX_USE_WCHAR_T
  template time_base::dateorder
  __time_get_dateorder<wchar_t>(current_abi, const facet*);
#endif

  template<typename C>
    istreambuf_iterator<C>
    __time_get(current_abi, const facet* f,
	       istreambuf_iterator<C> beg, istreambuf_iterator<C> end,
	       ios_base& io, ios_base::iostate& err, tm* t, char which)
    {
      auto* g = static_cast<const time_get<C>*>(f);
      switch(which)
      {
      case 't':
	return g->get_time(beg, end, io, err, t);
      case 'd':
	return g->get_date(beg, end, io, err, t);
      case 'w':
	return g->get_weekday(beg, end, io, err, t);
      case 'm':
	return g->get_monthname(beg, end, io, err, t);
      case 'y':
	return g->get_year(beg, end, io, err, t);
      default:
	__builtin_unreachable();
      }
    }

  template istreambuf_iterator<char>
  __time_get(current_abi, const facet*,
	     istreambuf_iterator<char>, istreambuf_iterator<char>,
	     ios_base&, ios_base::iostate&, tm*, char);

#ifdef _GLIBCXX_USE_WCHAR_T
  template istreambuf_iterator<wchar_t>
  __time_get(current_abi, const facet*,
	     istreambuf_iterator<wchar_t>, istreambuf_iterator<wchar_t>,
	     ios_base&, ios_base::iostate&, tm*, char);
#endif

  template<typename C>
    istreambuf_iterator<C>
    __money_get(current_abi, const facet* f,
		istreambuf_iterator<C> s, istreambuf_iterator<C> end,
		bool intl, ios_base& str, ios_base::iostate& err,
		long double* units, __any_string* digits)
    {
      auto* m = static_cast<const money_get<C>*>(f);
      if (units)
	return m->get(s, end, intl, str, err, *units);
      basic_string<C> digits2;
      s = m->get(s, end, intl, str, err, digits2);
      if (err == ios_base::goodbit)
	*digits = digits2;
      return s;
    }

  template istreambuf_iterator<char>
  __money_get(current_abi, const facet*,
	      istreambuf_iterator<char>, istreambuf_iterator<char>,
	      bool, ios_base&, ios_base::iostate&,
	      long double*, __any_string*);

#ifdef _GLIBCXX_USE_WCHAR_T
  template istreambuf_iterator<wchar_t>
  __money_get(current_abi, const facet*,
	      istreambuf_iterator<wchar_t>, istreambuf_iterator<wchar_t>,
	      bool, ios_base&, ios_base::iostate&,
	      long double*, __any_string*);
#endif

  template<typename C>
    ostreambuf_iterator<C>
    __money_put(current_abi, const facet* f, ostreambuf_iterator<C> s,
		bool intl, ios_base& io, C fill, long double units,
		const __any_string* digits)
    {
      auto* m = static_cast<const money_put<C>*>(f);
      if (digits)
	return m->put(s, intl, io, fill, *digits);
      else
	return m->put(s, intl, io, fill, units);
    }

  template ostreambuf_iterator<char>
  __money_put(current_abi, const facet*, ostreambuf_iterator<char>,
		bool, ios_base&, char, long double, const __any_string*);

#ifdef _GLIBCXX_USE_WCHAR_T
  template ostreambuf_iterator<wchar_t>
  __money_put(current_abi, const facet*, ostreambuf_iterator<wchar_t>,
		bool, ios_base&, wchar_t, long double, const __any_string*);
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace __facet_shims

_GLIBCXX_BEGIN_NAMESPACE_VERSION
  // Create a new shim facet of type WHICH that forwards calls to F.
  // F is the replacement facet provided by the user, WHICH is the ID of
  // F's "other ABI twin" which we are replacing with a shim.
  const locale::facet*
#if _GLIBCXX_USE_CXX11_ABI
  locale::facet::_M_sso_shim(const locale::id* which) const
#else
  locale::facet::_M_cow_shim(const locale::id* which) const
#endif
  {
    using namespace __facet_shims;

#if __cpp_rtti
    // If this is already a shim just use its underlying facet.
    if (auto* p = dynamic_cast<const __shim*>(this))
      return p->_M_get();
#endif

    if (which == &numpunct<char>::id)
      return new numpunct_shim<char>{this};
    if (which == &std::collate<char>::id)
      return new collate_shim<char>{this};
    if (which == &time_get<char>::id)
      return new time_get_shim<char>{this};
    if (which == &money_get<char>::id)
      return new money_get_shim<char>{this};
    if (which == &money_put<char>::id)
      return new money_put_shim<char>{this};
    if (which == &moneypunct<char, true>::id)
      return new moneypunct_shim<char, true>{this};
    if (which == &moneypunct<char, false>::id)
      return new moneypunct_shim<char, false>{this};
    if (which == &std::messages<char>::id)
      return new messages_shim<char>{this};
#ifdef _GLIBCXX_USE_WCHAR_T
    if (which == &numpunct<wchar_t>::id)
      return new numpunct_shim<wchar_t>{this};
    if (which == &std::collate<wchar_t>::id)
      return new collate_shim<wchar_t>{this};
    if (which == &time_get<wchar_t>::id)
      return new time_get_shim<wchar_t>{this};
    if (which == &money_get<wchar_t>::id)
      return new money_get_shim<wchar_t>{this};
    if (which == &money_put<wchar_t>::id)
      return new money_put_shim<wchar_t>{this};
    if (which == &moneypunct<wchar_t, true>::id)
      return new moneypunct_shim<wchar_t, true>{this};
    if (which == &moneypunct<wchar_t, false>::id)
      return new moneypunct_shim<wchar_t, false>{this};
    if (which == &std::messages<wchar_t>::id)
      return new messages_shim<wchar_t>{this};
#endif
    __throw_logic_error("cannot create shim for unknown locale::facet");
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
