// std::moneypunct implementation details, GNU version -*- C++ -*-

// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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
// ISO C++ 14882: 22.2.6.3.2  moneypunct virtual functions
//

// Written by Benjamin Kosnik <bkoz@redhat.com>

#include <locale>
#include <bits/c++locale_internal.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

// This file might be compiled twice, but we only want to define the members
// of money_base once.
#if ! _GLIBCXX_USE_CXX11_ABI

  // Construct and return valid pattern consisting of some combination of:
  // space none symbol sign value
  money_base::pattern
  money_base::_S_construct_pattern(char __precedes, char __space,
				   char __posn) throw()
  {
    pattern __ret;

    // This insanely complicated routine attempts to construct a valid
    // pattern for use with monyepunct. A couple of invariants:

    // if (__precedes) symbol -> value
    // else value -> symbol

    // if (__space) space
    // else none

    // none == never first
    // space never first or last

    // Any elegant implementations of this are welcome.
    switch (__posn)
      {
      case 0:
      case 1:
	// 1 The sign precedes the value and symbol.
	__ret.field[0] = sign;
	if (__space)
	  {
	    // Pattern starts with sign.
	    if (__precedes)
	      {
		__ret.field[1] = symbol;
		__ret.field[3] = value;
	      }
	    else
	      {
		__ret.field[1] = value;
		__ret.field[3] = symbol;
	      }
	    __ret.field[2] = space;
	  }
	else
	  {
	    // Pattern starts with sign and ends with none.
	    if (__precedes)
	      {
		__ret.field[1] = symbol;
		__ret.field[2] = value;
	      }
	    else
	      {
		__ret.field[1] = value;
		__ret.field[2] = symbol;
	      }
	    __ret.field[3] = none;
	  }
	break;
      case 2:
	// 2 The sign follows the value and symbol.
	if (__space)
	  {
	    // Pattern either ends with sign.
	    if (__precedes)
	      {
		__ret.field[0] = symbol;
		__ret.field[2] = value;
	      }
	    else
	      {
		__ret.field[0] = value;
		__ret.field[2] = symbol;
	      }
	    __ret.field[1] = space;
	    __ret.field[3] = sign;
	  }
	else
	  {
	    // Pattern ends with sign then none.
	    if (__precedes)
	      {
		__ret.field[0] = symbol;
		__ret.field[1] = value;
	      }
	    else
	      {
		__ret.field[0] = value;
		__ret.field[1] = symbol;
	      }
	    __ret.field[2] = sign;
	    __ret.field[3] = none;
	  }
	break;
      case 3:
	// 3 The sign immediately precedes the symbol.
	if (__precedes)
	  {
	    __ret.field[0] = sign;
	    __ret.field[1] = symbol;
	    if (__space)
	      {
		__ret.field[2] = space;
		__ret.field[3] = value;
	      }
	    else
	      {
		__ret.field[2] = value;
		__ret.field[3] = none;
	      }
	  }
	else
	  {
	    __ret.field[0] = value;
	    if (__space)
	      {
		__ret.field[1] = space;
		__ret.field[2] = sign;
		__ret.field[3] = symbol;
	      }
	    else
	      {
		__ret.field[1] = sign;
		__ret.field[2] = symbol;
		__ret.field[3] = none;
	      }
	  }
	break;
      case 4:
	// 4 The sign immediately follows the symbol.
	if (__precedes)
	  {
	    __ret.field[0] = symbol;
	    __ret.field[1] = sign;
	    if (__space)
	      {
		__ret.field[2] = space;
		__ret.field[3] = value;
	      }
	    else
	      {
		__ret.field[2] = value;
		__ret.field[3] = none;
	      }
	  }
	else
	  {
	    __ret.field[0] = value;
	    if (__space)
	      {
		__ret.field[1] = space;
		__ret.field[2] = symbol;
		__ret.field[3] = sign;
	      }
	    else
	      {
		__ret.field[1] = symbol;
		__ret.field[2] = sign;
		__ret.field[3] = none;
	      }
	  }
	break;
      default:
	__ret = pattern();
      }
    return __ret;
  }
#endif

  extern char __narrow_multibyte_chars(const char* s, __locale_t cloc);

  template<>
    void
    moneypunct<char, true>::_M_initialize_moneypunct(__c_locale __cloc,
						     const char*)
    {
      if (!_M_data)
	_M_data = new __moneypunct_cache<char, true>;

      if (!__cloc)
	{
	  // "C" locale
	  _M_data->_M_decimal_point = '.';
	  _M_data->_M_thousands_sep = ',';
	  _M_data->_M_grouping = "";
	  _M_data->_M_grouping_size = 0;
	  _M_data->_M_use_grouping = false;
	  _M_data->_M_curr_symbol = "";
	  _M_data->_M_curr_symbol_size = 0;
	  _M_data->_M_positive_sign = "";
	  _M_data->_M_positive_sign_size = 0;
	  _M_data->_M_negative_sign = "";
	  _M_data->_M_negative_sign_size = 0;
	  _M_data->_M_frac_digits = 0;
	  _M_data->_M_pos_format = money_base::_S_default_pattern;
	  _M_data->_M_neg_format = money_base::_S_default_pattern;

	  for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	    _M_data->_M_atoms[__i] = money_base::_S_atoms[__i];
	}
      else
	{
	  // Named locale.
	  _M_data->_M_decimal_point = *(__nl_langinfo_l(__MON_DECIMAL_POINT,
							__cloc));
	  const char* thousands_sep = __nl_langinfo_l(__MON_THOUSANDS_SEP,
						      __cloc);
	  if (thousands_sep[0] != '\0' && thousands_sep[1] != '\0')
	    _M_data->_M_thousands_sep = __narrow_multibyte_chars(thousands_sep,
								 __cloc);
	  else
	    _M_data->_M_thousands_sep = *thousands_sep;

	  // Check for NULL, which implies no fractional digits.
	  if (_M_data->_M_decimal_point == '\0')
	    {
	      // Like in "C" locale.
	      _M_data->_M_frac_digits = 0;
	      _M_data->_M_decimal_point = '.';
	    }
	  else
	    _M_data->_M_frac_digits = *(__nl_langinfo_l(__INT_FRAC_DIGITS,
							__cloc));

	  const char* __cgroup = __nl_langinfo_l(__MON_GROUPING, __cloc);
	  const char* __cpossign = __nl_langinfo_l(__POSITIVE_SIGN, __cloc);
	  const char* __cnegsign = __nl_langinfo_l(__NEGATIVE_SIGN, __cloc);
	  // _Intl == true
	  const char* __ccurr = __nl_langinfo_l(__INT_CURR_SYMBOL, __cloc);

	  char* __group = 0;
	  char* __ps = 0;
	  char* __ns = 0;
	  const char __nposn = *(__nl_langinfo_l(__INT_N_SIGN_POSN, __cloc));
	  __try
	    {
	      size_t __len;

	      // Check for NULL, which implies no grouping.
	      if (_M_data->_M_thousands_sep == '\0')
		{
		  // Like in "C" locale.
		  _M_data->_M_grouping = "";
		  _M_data->_M_grouping_size = 0;
		  _M_data->_M_use_grouping = false;
		  _M_data->_M_thousands_sep = ',';
		}
	      else
		{
		  __len = strlen(__cgroup);
		  if (__len)
		    {
		      __group = new char[__len + 1];
		      memcpy(__group, __cgroup, __len + 1);
		      _M_data->_M_grouping = __group;
		    }
		  else
		    {
		      _M_data->_M_grouping = "";
		      _M_data->_M_use_grouping = false;
		    }
		  _M_data->_M_grouping_size = __len;
		}

	      __len = strlen(__cpossign);
	      if (__len)
		{
		  __ps = new char[__len + 1];
		  memcpy(__ps, __cpossign, __len + 1);
		  _M_data->_M_positive_sign = __ps;
		}
	      else
		_M_data->_M_positive_sign = "";
	      _M_data->_M_positive_sign_size = __len;

	      if (!__nposn)
		{
		  _M_data->_M_negative_sign = "()";
		  _M_data->_M_negative_sign_size = 2;
		}
	      else
		{
		  __len = strlen(__cnegsign);
		  if (__len)
		    {
		      __ns = new char[__len + 1];
		      memcpy(__ns, __cnegsign, __len + 1);
		      _M_data->_M_negative_sign = __ns;
		    }
		  else
		    _M_data->_M_negative_sign = "";
		  _M_data->_M_negative_sign_size = __len;
		}

	      __len = strlen(__ccurr);
	      if (__len)
		{
		  char* __curr = new char[__len + 1];
		  memcpy(__curr, __ccurr, __len + 1);
		  _M_data->_M_curr_symbol = __curr;
		}
	      else
		_M_data->_M_curr_symbol = "";
	      _M_data->_M_curr_symbol_size = __len;
	    }
	  __catch(...)
	    {
	      delete _M_data;
	      _M_data = 0;
	      delete [] __group;
	      delete [] __ps;
	      delete [] __ns;
	      __throw_exception_again;
	    }

	  char __pprecedes = *(__nl_langinfo_l(__INT_P_CS_PRECEDES, __cloc));
	  char __pspace = *(__nl_langinfo_l(__INT_P_SEP_BY_SPACE, __cloc));
	  char __pposn = *(__nl_langinfo_l(__INT_P_SIGN_POSN, __cloc));
	  _M_data->_M_pos_format = _S_construct_pattern(__pprecedes, __pspace,
							__pposn);
	  char __nprecedes = *(__nl_langinfo_l(__INT_N_CS_PRECEDES, __cloc));
	  char __nspace = *(__nl_langinfo_l(__INT_N_SEP_BY_SPACE, __cloc));
	  _M_data->_M_neg_format = _S_construct_pattern(__nprecedes, __nspace,
							__nposn);
	}
    }

  template<>
    void
    moneypunct<char, false>::_M_initialize_moneypunct(__c_locale __cloc,
						      const char*)
    {
      if (!_M_data)
	_M_data = new __moneypunct_cache<char, false>;

      if (!__cloc)
	{
	  // "C" locale
	  _M_data->_M_decimal_point = '.';
	  _M_data->_M_thousands_sep = ',';
	  _M_data->_M_grouping = "";
	  _M_data->_M_grouping_size = 0;
	  _M_data->_M_use_grouping = false;
	  _M_data->_M_curr_symbol = "";
	  _M_data->_M_curr_symbol_size = 0;
	  _M_data->_M_positive_sign = "";
	  _M_data->_M_positive_sign_size = 0;
	  _M_data->_M_negative_sign = "";
	  _M_data->_M_negative_sign_size = 0;
	  _M_data->_M_frac_digits = 0;
	  _M_data->_M_pos_format = money_base::_S_default_pattern;
	  _M_data->_M_neg_format = money_base::_S_default_pattern;

	  for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	    _M_data->_M_atoms[__i] = money_base::_S_atoms[__i];
	}
      else
	{
	  // Named locale.
	  _M_data->_M_decimal_point = *(__nl_langinfo_l(__MON_DECIMAL_POINT,
							__cloc));
	  _M_data->_M_thousands_sep = *(__nl_langinfo_l(__MON_THOUSANDS_SEP,
							__cloc));

	  // Check for NULL, which implies no fractional digits.
	  if (_M_data->_M_decimal_point == '\0')
	    {
	      // Like in "C" locale.
	      _M_data->_M_frac_digits = 0;
	      _M_data->_M_decimal_point = '.';
	    }
	  else
	    _M_data->_M_frac_digits = *(__nl_langinfo_l(__FRAC_DIGITS,
							__cloc));

	  const char* __cgroup = __nl_langinfo_l(__MON_GROUPING, __cloc);
	  const char* __cpossign = __nl_langinfo_l(__POSITIVE_SIGN, __cloc);
	  const char* __cnegsign = __nl_langinfo_l(__NEGATIVE_SIGN, __cloc);
	  // _Intl == false
	  const char* __ccurr = __nl_langinfo_l(__CURRENCY_SYMBOL, __cloc);

	  char* __group = 0;
	  char* __ps = 0;
	  char* __ns = 0;
	  const char __nposn = *(__nl_langinfo_l(__N_SIGN_POSN, __cloc));
	  __try
	    {
	      size_t __len;

	      // Check for NULL, which implies no grouping.
	      if (_M_data->_M_thousands_sep == '\0')
		{
		  // Like in "C" locale.
		  _M_data->_M_grouping = "";
		  _M_data->_M_grouping_size = 0;
		  _M_data->_M_use_grouping = false;
		  _M_data->_M_thousands_sep = ',';
		}
	      else
		{
		  __len = strlen(__cgroup);
		  if (__len)
		    {
		      __group = new char[__len + 1];
		      memcpy(__group, __cgroup, __len + 1);
		      _M_data->_M_grouping = __group;
		    }
		  else
		    {
		      _M_data->_M_grouping = "";
		      _M_data->_M_use_grouping = false;
		    }
		  _M_data->_M_grouping_size = __len;
		}

	      __len = strlen(__cpossign);
	      if (__len)
		{
		  __ps = new char[__len + 1];
		  memcpy(__ps, __cpossign, __len + 1);
		  _M_data->_M_positive_sign = __ps;
		}
	      else
		_M_data->_M_positive_sign = "";
	      _M_data->_M_positive_sign_size = __len;

	      if (!__nposn)
		{
		  _M_data->_M_negative_sign = "()";
		  _M_data->_M_negative_sign_size = 2;
		}
	      else
		{
		  __len = strlen(__cnegsign);
		  if (__len)
		    {
		      __ns = new char[__len + 1];
		      memcpy(__ns, __cnegsign, __len + 1);
		      _M_data->_M_negative_sign = __ns;
		    }
		  else
		    _M_data->_M_negative_sign = "";
		  _M_data->_M_negative_sign_size = __len;
		}

	      __len = strlen(__ccurr);
	      if (__len)
		{
		  char* __curr = new char[__len + 1];
		  memcpy(__curr, __ccurr, __len + 1);
		  _M_data->_M_curr_symbol = __curr;
		}
	      else
		_M_data->_M_curr_symbol = "";
	      _M_data->_M_curr_symbol_size = __len;
	    }
	  __catch(...)
	    {
	      delete _M_data;
	      _M_data = 0;
	      delete [] __group;
	      delete [] __ps;
	      delete [] __ns;
	      __throw_exception_again;
	    }

	  char __pprecedes = *(__nl_langinfo_l(__P_CS_PRECEDES, __cloc));
	  char __pspace = *(__nl_langinfo_l(__P_SEP_BY_SPACE, __cloc));
	  char __pposn = *(__nl_langinfo_l(__P_SIGN_POSN, __cloc));
	  _M_data->_M_pos_format = _S_construct_pattern(__pprecedes, __pspace,
							__pposn);
	  char __nprecedes = *(__nl_langinfo_l(__N_CS_PRECEDES, __cloc));
	  char __nspace = *(__nl_langinfo_l(__N_SEP_BY_SPACE, __cloc));
	  _M_data->_M_neg_format = _S_construct_pattern(__nprecedes, __nspace,
							__nposn);
	}
    }

  template<>
    moneypunct<char, true>::~moneypunct()
    {
      if (_M_data->_M_grouping_size)
	delete [] _M_data->_M_grouping;
      if (_M_data->_M_positive_sign_size)
	delete [] _M_data->_M_positive_sign;
      if (_M_data->_M_negative_sign_size
          && strcmp(_M_data->_M_negative_sign, "()") != 0)
	delete [] _M_data->_M_negative_sign;
      if (_M_data->_M_curr_symbol_size)
	delete [] _M_data->_M_curr_symbol;
      delete _M_data;
    }

  template<>
    moneypunct<char, false>::~moneypunct()
    {
      if (_M_data->_M_grouping_size)
	delete [] _M_data->_M_grouping;
      if (_M_data->_M_positive_sign_size)
	delete [] _M_data->_M_positive_sign;
      if (_M_data->_M_negative_sign_size
          && strcmp(_M_data->_M_negative_sign, "()") != 0)
	delete [] _M_data->_M_negative_sign;
      if (_M_data->_M_curr_symbol_size)
	delete [] _M_data->_M_curr_symbol;
      delete _M_data;
    }

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    void
    moneypunct<wchar_t, true>::_M_initialize_moneypunct(__c_locale __cloc,
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
							const char*)
#else
							const char* __name)
#endif
    {
      if (!_M_data)
	_M_data = new __moneypunct_cache<wchar_t, true>;

      if (!__cloc)
	{
	  // "C" locale
	  _M_data->_M_decimal_point = L'.';
	  _M_data->_M_thousands_sep = L',';
	  _M_data->_M_grouping = "";
	  _M_data->_M_grouping_size = 0;
	  _M_data->_M_use_grouping = false;
	  _M_data->_M_curr_symbol = L"";
	  _M_data->_M_curr_symbol_size = 0;
	  _M_data->_M_positive_sign = L"";
	  _M_data->_M_positive_sign_size = 0;
	  _M_data->_M_negative_sign = L"";
	  _M_data->_M_negative_sign_size = 0;
	  _M_data->_M_frac_digits = 0;
	  _M_data->_M_pos_format = money_base::_S_default_pattern;
	  _M_data->_M_neg_format = money_base::_S_default_pattern;

	  // Use ctype::widen code without the facet...
	  for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	    _M_data->_M_atoms[__i] =
	      static_cast<wchar_t>(money_base::_S_atoms[__i]);
	}
      else
	{
	  // Named locale.
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
	  __c_locale __old = __uselocale(__cloc);
#else
	  // Switch to named locale so that mbsrtowcs will work.
	  char* __old = setlocale(LC_ALL, 0);
          const size_t __llen = strlen(__old) + 1;
          char* __sav = new char[__llen];
          memcpy(__sav, __old, __llen);
	  setlocale(LC_ALL, __name);
#endif

	  union { char *__s; wchar_t __w; } __u;
	  __u.__s = __nl_langinfo_l(_NL_MONETARY_DECIMAL_POINT_WC, __cloc);
	  _M_data->_M_decimal_point = __u.__w;

	  __u.__s = __nl_langinfo_l(_NL_MONETARY_THOUSANDS_SEP_WC, __cloc);
	  _M_data->_M_thousands_sep = __u.__w;

	  // Check for NULL, which implies no fractional digits.
	  if (_M_data->_M_decimal_point == L'\0')
	    {
	      // Like in "C" locale.
	      _M_data->_M_frac_digits = 0;
	      _M_data->_M_decimal_point = L'.';
	    }
	  else
	    _M_data->_M_frac_digits = *(__nl_langinfo_l(__INT_FRAC_DIGITS,
							__cloc));

	  const char* __cgroup = __nl_langinfo_l(__MON_GROUPING, __cloc);
	  const char* __cpossign = __nl_langinfo_l(__POSITIVE_SIGN, __cloc);
	  const char* __cnegsign = __nl_langinfo_l(__NEGATIVE_SIGN, __cloc);
	  const char* __ccurr = __nl_langinfo_l(__INT_CURR_SYMBOL, __cloc);

	  char* __group = 0;
	  wchar_t* __wcs_ps = 0;
	  wchar_t* __wcs_ns = 0;
	  const char __nposn = *(__nl_langinfo_l(__INT_N_SIGN_POSN, __cloc));
	  __try
	    {
	      size_t __len;

	      // Check for NULL, which implies no grouping.
	      if (_M_data->_M_thousands_sep == L'\0')
		{
		  // Like in "C" locale.
		  _M_data->_M_grouping = "";
		  _M_data->_M_grouping_size = 0;
		  _M_data->_M_use_grouping = false;
		  _M_data->_M_thousands_sep = L',';
		}
	      else
		{
		  __len = strlen(__cgroup);
		  if (__len)
		    {
		      __group = new char[__len + 1];
		      memcpy(__group, __cgroup, __len + 1);
		      _M_data->_M_grouping = __group;
		    }
		  else
		    {
		      _M_data->_M_grouping = "";
		      _M_data->_M_use_grouping = false;
		    }
		  _M_data->_M_grouping_size = __len;
		}

	      mbstate_t __state;
	      __len = strlen(__cpossign);
	      if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  __wcs_ps = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs_ps, &__cpossign, __len + 1, &__state);
		  _M_data->_M_positive_sign = __wcs_ps;
		}
	      else
		_M_data->_M_positive_sign = L"";
	      _M_data->_M_positive_sign_size =
		wcslen(_M_data->_M_positive_sign);

	      __len = strlen(__cnegsign);
	      if (!__nposn)
		_M_data->_M_negative_sign = L"()";
	      else if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  __wcs_ns = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs_ns, &__cnegsign, __len + 1, &__state);
		  _M_data->_M_negative_sign = __wcs_ns;
		}
	      else
		_M_data->_M_negative_sign = L"";
	      _M_data->_M_negative_sign_size =
		wcslen(_M_data->_M_negative_sign);

	      // _Intl == true.
	      __len = strlen(__ccurr);
	      if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  wchar_t* __wcs = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs, &__ccurr, __len + 1, &__state);
		  _M_data->_M_curr_symbol = __wcs;
		}
	      else
		_M_data->_M_curr_symbol = L"";
	      _M_data->_M_curr_symbol_size = wcslen(_M_data->_M_curr_symbol);
	    }
	  __catch(...)
	    {
	      delete _M_data;
	      _M_data = 0;
	      delete [] __group;
	      delete [] __wcs_ps;
	      delete [] __wcs_ns;
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
	      __uselocale(__old);
#else
	      setlocale(LC_ALL, __sav);
	      delete [] __sav;
#endif
	      __throw_exception_again;
	    }

	  char __pprecedes = *(__nl_langinfo_l(__INT_P_CS_PRECEDES, __cloc));
	  char __pspace = *(__nl_langinfo_l(__INT_P_SEP_BY_SPACE, __cloc));
	  char __pposn = *(__nl_langinfo_l(__INT_P_SIGN_POSN, __cloc));
	  _M_data->_M_pos_format = _S_construct_pattern(__pprecedes, __pspace,
							__pposn);
	  char __nprecedes = *(__nl_langinfo_l(__INT_N_CS_PRECEDES, __cloc));
	  char __nspace = *(__nl_langinfo_l(__INT_N_SEP_BY_SPACE, __cloc));
	  _M_data->_M_neg_format = _S_construct_pattern(__nprecedes, __nspace,
							__nposn);

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
	  __uselocale(__old);
#else
	  setlocale(LC_ALL, __sav);
	  delete [] __sav;
#endif
	}
    }

  template<>
  void
  moneypunct<wchar_t, false>::_M_initialize_moneypunct(__c_locale __cloc,
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
						       const char*)
#else
                                                       const char* __name)
#endif
  {
    if (!_M_data)
      _M_data = new __moneypunct_cache<wchar_t, false>;

    if (!__cloc)
	{
	  // "C" locale
	  _M_data->_M_decimal_point = L'.';
	  _M_data->_M_thousands_sep = L',';
	  _M_data->_M_grouping = "";
          _M_data->_M_grouping_size = 0;
	  _M_data->_M_use_grouping = false;
	  _M_data->_M_curr_symbol = L"";
	  _M_data->_M_curr_symbol_size = 0;
	  _M_data->_M_positive_sign = L"";
	  _M_data->_M_positive_sign_size = 0;
	  _M_data->_M_negative_sign = L"";
	  _M_data->_M_negative_sign_size = 0;
	  _M_data->_M_frac_digits = 0;
	  _M_data->_M_pos_format = money_base::_S_default_pattern;
	  _M_data->_M_neg_format = money_base::_S_default_pattern;

	  // Use ctype::widen code without the facet...
	  for (size_t __i = 0; __i < money_base::_S_end; ++__i)
	    _M_data->_M_atoms[__i] =
	      static_cast<wchar_t>(money_base::_S_atoms[__i]);
	}
      else
	{
	  // Named locale.
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
	  __c_locale __old = __uselocale(__cloc);
#else
	  // Switch to named locale so that mbsrtowcs will work.
	  char* __old = setlocale(LC_ALL, 0);
          const size_t __llen = strlen(__old) + 1;
          char* __sav = new char[__llen];
          memcpy(__sav, __old, __llen);
	  setlocale(LC_ALL, __name);
#endif

          union { char *__s; wchar_t __w; } __u;
	  __u.__s = __nl_langinfo_l(_NL_MONETARY_DECIMAL_POINT_WC, __cloc);
	  _M_data->_M_decimal_point = __u.__w;

	  __u.__s = __nl_langinfo_l(_NL_MONETARY_THOUSANDS_SEP_WC, __cloc);
	  _M_data->_M_thousands_sep = __u.__w;

	  // Check for NULL, which implies no fractional digits.
	  if (_M_data->_M_decimal_point == L'\0')
	    {
	      // Like in "C" locale.
	      _M_data->_M_frac_digits = 0;
	      _M_data->_M_decimal_point = L'.';
	    }
	  else
	    _M_data->_M_frac_digits = *(__nl_langinfo_l(__FRAC_DIGITS,
							__cloc));

	  const char* __cgroup = __nl_langinfo_l(__MON_GROUPING, __cloc);
	  const char* __cpossign = __nl_langinfo_l(__POSITIVE_SIGN, __cloc);
	  const char* __cnegsign = __nl_langinfo_l(__NEGATIVE_SIGN, __cloc);
	  const char* __ccurr = __nl_langinfo_l(__CURRENCY_SYMBOL, __cloc);

	  char* __group = 0;
	  wchar_t* __wcs_ps = 0;
	  wchar_t* __wcs_ns = 0;
	  const char __nposn = *(__nl_langinfo_l(__N_SIGN_POSN, __cloc));
	  __try
            {
	      size_t __len;

	      // Check for NULL, which implies no grouping.
	      if (_M_data->_M_thousands_sep == L'\0')
		{
		  // Like in "C" locale.
		  _M_data->_M_grouping = "";
		  _M_data->_M_grouping_size = 0;
		  _M_data->_M_use_grouping = false;
		  _M_data->_M_thousands_sep = L',';
		}
	      else
		{
		  __len = strlen(__cgroup);
		  if (__len)
		    {
		      __group = new char[__len + 1];
		      memcpy(__group, __cgroup, __len + 1);
		      _M_data->_M_grouping = __group;
		    }
		  else
		    {
		      _M_data->_M_grouping = "";
		      _M_data->_M_use_grouping = false;
		    }
		  _M_data->_M_grouping_size = __len;
		}

              mbstate_t __state;
              __len = strlen(__cpossign);
              if (__len)
                {
		  memset(&__state, 0, sizeof(mbstate_t));
		  __wcs_ps = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs_ps, &__cpossign, __len + 1, &__state);
		  _M_data->_M_positive_sign = __wcs_ps;
		}
	      else
		_M_data->_M_positive_sign = L"";
              _M_data->_M_positive_sign_size =
		wcslen(_M_data->_M_positive_sign);

	      __len = strlen(__cnegsign);
	      if (!__nposn)
		_M_data->_M_negative_sign = L"()";
	      else if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  __wcs_ns = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs_ns, &__cnegsign, __len + 1, &__state);
		  _M_data->_M_negative_sign = __wcs_ns;
		}
	      else
		_M_data->_M_negative_sign = L"";
              _M_data->_M_negative_sign_size =
		wcslen(_M_data->_M_negative_sign);

	      // _Intl == true.
	      __len = strlen(__ccurr);
	      if (__len)
		{
		  memset(&__state, 0, sizeof(mbstate_t));
		  wchar_t* __wcs = new wchar_t[__len + 1];
		  mbsrtowcs(__wcs, &__ccurr, __len + 1, &__state);
		  _M_data->_M_curr_symbol = __wcs;
		}
	      else
		_M_data->_M_curr_symbol = L"";
              _M_data->_M_curr_symbol_size = wcslen(_M_data->_M_curr_symbol);
	    }
          __catch(...)
	    {
	      delete _M_data;
              _M_data = 0;
	      delete [] __group;
	      delete [] __wcs_ps;
	      delete [] __wcs_ns;
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
	      __uselocale(__old);
#else
	      setlocale(LC_ALL, __sav);
	      delete [] __sav;
#endif
              __throw_exception_again;
	    }

	  char __pprecedes = *(__nl_langinfo_l(__P_CS_PRECEDES, __cloc));
	  char __pspace = *(__nl_langinfo_l(__P_SEP_BY_SPACE, __cloc));
	  char __pposn = *(__nl_langinfo_l(__P_SIGN_POSN, __cloc));
	  _M_data->_M_pos_format = _S_construct_pattern(__pprecedes, __pspace,
	                                                __pposn);
	  char __nprecedes = *(__nl_langinfo_l(__N_CS_PRECEDES, __cloc));
	  char __nspace = *(__nl_langinfo_l(__N_SEP_BY_SPACE, __cloc));
	  _M_data->_M_neg_format = _S_construct_pattern(__nprecedes, __nspace,
	                                                __nposn);

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
	  __uselocale(__old);
#else
	  setlocale(LC_ALL, __sav);
	  delete [] __sav;
#endif
	}
    }

  template<>
    moneypunct<wchar_t, true>::~moneypunct()
    {
      if (_M_data->_M_grouping_size)
	delete [] _M_data->_M_grouping;
      if (_M_data->_M_positive_sign_size)
	delete [] _M_data->_M_positive_sign;
      if (_M_data->_M_negative_sign_size
          && wcscmp(_M_data->_M_negative_sign, L"()") != 0)
	delete [] _M_data->_M_negative_sign;
      if (_M_data->_M_curr_symbol_size)
	delete [] _M_data->_M_curr_symbol;
      delete _M_data;
    }

  template<>
    moneypunct<wchar_t, false>::~moneypunct()
    {
      if (_M_data->_M_grouping_size)
	delete [] _M_data->_M_grouping;
      if (_M_data->_M_positive_sign_size)
	delete [] _M_data->_M_positive_sign;
      if (_M_data->_M_negative_sign_size
          && wcscmp(_M_data->_M_negative_sign, L"()") != 0)
	delete [] _M_data->_M_negative_sign;
      if (_M_data->_M_curr_symbol_size)
	delete [] _M_data->_M_curr_symbol;
      delete _M_data;
    }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
